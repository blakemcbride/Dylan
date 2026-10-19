/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Startup, errors, the static objects, and the simplest constructors.
 */

#include "dylan.h"
#include <stdio.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>

DyHeader dy_true_object = { &dy_wrapper_boolean };
DyHeader dy_false_object = { &dy_wrapper_boolean };
DyHeader dy_empty_list_object = { &dy_wrapper_empty_list };

void dy_intern_static_symbols(void);    /* string.c */
void dy_init_main_thread(void);         /* thread.c */

static long gc_stress_interval = 100;
static long gc_stress_count;

void dy_gc_stress(void) {
    if (__atomic_add_fetch(&gc_stress_count, 1, __ATOMIC_RELAXED) % gc_stress_interval == 0)
        GC_gcollect();
}

void dy_init(void) {
    setlocale(LC_CTYPE, "C.UTF-8");        /* for the case of characters */
    GC_INIT();
#ifdef DY_GC_STRESS
    const char *interval = getenv("DYLAN_GC_STRESS");
    if (interval && atol(interval) > 0) gc_stress_interval = atol(interval);
#endif
    GC_allow_register_threads();
    dy_init_thread();
    dy_init_main_thread();
    dy_intern_static_symbols();
}

_Noreturn void dy_integer_overflow(void) {
    dy_error("integer overflow (integers are 63 bits)", 0);
}

_Noreturn void dy_division_by_zero(void) {
    dy_error("division by zero", 0);
}

_Noreturn void dy_representation_error(const char *expected, D x) {
    char message[120];
    snprintf(message, sizeof message, "internal error: the compiled code expected %s", expected);
    dy_error(message, x);
}

D dy_pair(D head, D tail) {
    DyPair *p = dy_alloc(&dy_wrapper_pair, sizeof(DyPair));
    p->head = head;
    p->tail = tail;
    return DY_OBJ(p);
}

D dy_make_vector(size_t size, D fill) {
    if (size > (size_t)DY_INT_MAX / sizeof(D)) dy_error("vector too large", 0);
    DyVector *v = dy_alloc(&dy_wrapper_simple_object_vector,
                           sizeof(DyVector) + size * sizeof(D));
    v->size = DY_INT(size);
    for (size_t i = 0; i < size; i++) v->data[i] = fill;
    return DY_OBJ(v);
}

/* ---------------------------------------------------------- Debugging */

static void print_symbol_name(FILE *out, D name) {
    DyByteString *s = DY_PTR(name);
    fwrite(s->data, 1, DY_INT_VALUE(s->size), out);
}

static void write_number(FILE *out, D x) {
    DyByteString *s = DY_PTR(dy_number_string(x));
    fwrite(s->data, 1, DY_INT_VALUE(s->size), out);
}

void dy_debug_write(FILE *out, D x) {
    if (DY_IS_INT(x)) {
        fprintf(out, "%" PRIdPTR, DY_INT_VALUE(x));
        return;
    }
    if (DY_IS_CHAR(x)) {
        uint32_t c = DY_CHAR_CODE(x);
        if (c >= 32 && c < 127) fprintf(out, "#\\%c", (int)c);
        else fprintf(out, "#\\U+%04X", (unsigned)c);
        return;
    }
    if (x == DY_TRUE) { fputs("#t", out); return; }
    if (x == DY_FALSE) { fputs("#f", out); return; }
    if (x == DY_EMPTY) { fputs("()", out); return; }
    DyWrapper *w = dy_wrapper_of(x);
    switch (w->kind) {
    case DY_KIND_PAIR: {
        fputc('(', out);
        dy_debug_write(out, ((DyPair *)DY_PTR(x))->head);
        D rest = ((DyPair *)DY_PTR(x))->tail;
        while (dy_kind_p(rest, DY_KIND_PAIR)) {
            fputc(' ', out);
            dy_debug_write(out, ((DyPair *)DY_PTR(rest))->head);
            rest = ((DyPair *)DY_PTR(rest))->tail;
        }
        if (rest != DY_EMPTY) {
            fputs(" . ", out);
            dy_debug_write(out, rest);
        }
        fputc(')', out);
        return;
    }
    case DY_KIND_RATIO:
    case DY_KIND_SINGLE_FLOAT:
    case DY_KIND_DOUBLE_FLOAT:
    case DY_KIND_EXTENDED_FLOAT:
        write_number(out, x);
        return;
    case DY_KIND_BYTE_STRING: {
        DyByteString *s = DY_PTR(x);
        fputc('"', out);
        fwrite(s->data, 1, DY_INT_VALUE(s->size), out);
        fputc('"', out);
        return;
    }
    case DY_KIND_UNICODE_STRING: {
        DyUnicodeString *s = DY_PTR(x);
        fputc('"', out);
        for (intptr_t i = 0; i < DY_INT_VALUE(s->size); i++) {
            uint32_t c = s->data[i];
            if (c < 128) fputc((int)c, out);
            else fprintf(out, "\\U+%04X", (unsigned)c);
        }
        fputc('"', out);
        return;
    }
    case DY_KIND_SIMPLE_OBJECT_VECTOR: {
        DyVector *v = DY_PTR(x);
        fputs("#(", out);
        for (intptr_t i = 0; i < DY_INT_VALUE(v->size); i++) {
            if (i) fputc(' ', out);
            dy_debug_write(out, v->data[i]);
        }
        fputc(')', out);
        return;
    }
    case DY_KIND_SYMBOL:
        print_symbol_name(out, ((DySymbol *)DY_PTR(x))->name);
        return;
    case DY_KIND_KEYWORD:
        print_symbol_name(out, ((DySymbol *)DY_PTR(x))->name);
        fputc(':', out);
        return;
    case DY_KIND_FUNCTION: {
        /* As the 1992 manual prints them (chapters 8, 10 and 11) */
        DyFunction *f = DY_PTR(x);
        if (f->owner != 0 && f->owner != DY_FALSE) {
            fputs("{a method on ", out);
            D name = ((DyGeneric *)DY_PTR(f->owner))->name;
            if (name == DY_FALSE) fputs("an anonymous generic function", out);
            else print_symbol_name(out, ((DySymbol *)DY_PTR(name))->name);
            fputc('}', out);
        } else if (f->name) {
            fprintf(out, "{method %s}", f->name);
        } else {
            fputs("{an anonymous method}", out);
        }
        return;
    }
    case DY_KIND_THREAD: fputs("{thread}", out); return;
    case DY_KIND_SLOT_DESCRIPTOR: fputs("{slot}", out); return;
    case DY_KIND_LOCK: fputs("{lock}", out); return;
    case DY_KIND_CONDITION_VARIABLE: fputs("{condition-variable}", out); return;
    case DY_KIND_ATOMIC_INTEGER:
        fprintf(out, "{atomic-integer %" PRIdPTR "}",
                (intptr_t)atomic_load(&((DyAtomicInteger *)DY_PTR(x))->value));
        return;
    case DY_KIND_GENERIC: {
        D name = ((DyGeneric *)DY_PTR(x))->name;
        if (name == DY_FALSE) {
            fputs("{an anonymous generic function}", out);
            return;
        }
        fputs("{the generic function ", out);
        print_symbol_name(out, ((DySymbol *)DY_PTR(name))->name);
        fputc('}', out);
        return;
    }
    case DY_KIND_CLASS:
        fputs("{the class ", out);
        print_symbol_name(out, ((DySymbol *)DY_PTR(((DyClass *)DY_PTR(x))->name))->name);
        fputc('}', out);
        return;
    default:
        fputs("{instance of ", out);
        print_symbol_name(out, ((DySymbol *)DY_PTR(((DyClass *)DY_PTR(w->class))->name))->name);
        fputc('}', out);
        return;
    }
}

void dy_debug_print(D x) { dy_debug_write(stdout, x); }
