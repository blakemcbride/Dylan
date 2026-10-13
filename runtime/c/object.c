/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Startup, errors, the static objects, and the simplest constructors.
 */

#include "dylan.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>

DyHeader dy_true_object = { &dy_wrapper_boolean };
DyHeader dy_false_object = { &dy_wrapper_boolean };
DyHeader dy_empty_list_object = { &dy_wrapper_empty_list };

void dy_intern_static_symbols(void);    /* string.c */
void dy_init_main_thread(void);         /* thread.c */

void dy_init(void) {
    GC_INIT();
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

/* A float, with a decimal point, and its format's exponent marker. */
static void write_float(FILE *out, double value, int digits, char marker) {
    char buffer[64];
    snprintf(buffer, sizeof buffer, "%.*g", digits, value);
    char *e = strchr(buffer, 'e');
    if (!strpbrk(buffer, ".ein")) strcat(buffer, ".0");
    if (e) {
        *e = '\0';
        fprintf(out, "%s%s%c%s", buffer, strchr(buffer, '.') ? "" : ".0", marker, e + 1);
    } else {
        fprintf(out, "%s%c0", buffer, marker);
    }
}

void dy_debug_write(FILE *out, D x) {
    if (DY_IS_INT(x)) {
        fprintf(out, "%ld", (long)DY_INT_VALUE(x));
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
    case DY_KIND_RATIO: {
        DyRatio *r = DY_PTR(x);
        fprintf(out, "%ld/%ld", (long)DY_INT_VALUE(r->numerator),
               (long)DY_INT_VALUE(r->denominator));
        return;
    }
    case DY_KIND_SINGLE_FLOAT:
        write_float(out, ((DySingleFloat *)DY_PTR(x))->value, 9, 's');
        return;
    case DY_KIND_DOUBLE_FLOAT:
        write_float(out, ((DyDoubleFloat *)DY_PTR(x))->value, 17, 'd');
        return;
    case DY_KIND_EXTENDED_FLOAT:
        write_float(out, ((DyDoubleFloat *)DY_PTR(x))->value, 17, 'l');
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
    case DY_KIND_FUNCTION:
        fputs("{method}", out);
        return;
    case DY_KIND_THREAD: fputs("{thread}", out); return;
    case DY_KIND_LOCK: fputs("{lock}", out); return;
    case DY_KIND_CONDITION_VARIABLE: fputs("{condition-variable}", out); return;
    case DY_KIND_ATOMIC_INTEGER:
        fprintf(out, "{atomic-integer %ld}",
                (long)atomic_load(&((DyAtomicInteger *)DY_PTR(x))->value));
        return;
    case DY_KIND_CLASS:
        fputs("{class ", out);
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
