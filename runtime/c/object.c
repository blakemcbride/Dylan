/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Startup, errors, the static objects, and the simplest constructors.
 */

#include "dylan.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>

DyHeader dy_true_object = { &dy_wrapper_boolean };
DyHeader dy_false_object = { &dy_wrapper_boolean };
DyHeader dy_empty_list_object = { &dy_wrapper_empty_list };

void dy_intern_static_symbols(void);    /* string.c */
void dy_init_main_thread(void);         /* thread.c */

/* Under AddressSanitizer, its fake stack -- frames kept apart from the
 * stack to catch uses after return -- would hide the references in them
 * from the collector, which scans the stack: an object referred to only
 * from there would be freed, or moved, while in use.  Clang turns it on
 * by default; this turns it off, unless ASAN_OPTIONS says otherwise. */
#ifndef __has_feature
#define __has_feature(x) 0
#endif
#if defined(__SANITIZE_ADDRESS__) || __has_feature(address_sanitizer)
const char *__asan_default_options(void) { return "detect_stack_use_after_return=0"; }
#endif

#ifndef DY_MPS
static long gc_stress_interval = 100;
static long gc_stress_count;

void dy_gc_stress(void) {
    if (__atomic_add_fetch(&gc_stress_count, 1, __ATOMIC_RELAXED) % gc_stress_interval == 0)
        GC_gcollect();
}

/* Weak lists (dylan.h): memory the collector does not scan, each object
 * in it registered as a disappearing link, which the collector clears to
 * 0 when the object dies (and then forgets).  A list let go must have its
 * links unregistered first, or the collector would clear them in memory
 * reused since. */
typedef struct {
    intptr_t size;                 /* room for objects                    */
    intptr_t used;                 /* how many of OBJECTS are used, one   */
    void *objects[];               /*   0 once its object has died        */
} WeakList;

static WeakList *make_weak_list(intptr_t size) {
    WeakList *l = GC_MALLOC_ATOMIC(sizeof(WeakList) + (size_t)size * sizeof(void *));
    if (!l) dy_error("out of memory", 0);
    memset(l, 0, sizeof(WeakList) + (size_t)size * sizeof(void *));
    l->size = size;
    return l;
}

/* Put X, the object at FROM (read into a variable, so alive), at TO, its
 * link with it */
static void move_weak(void *x, void **from, void **to) {
    if (from == to) return;
    GC_unregister_disappearing_link(from);
    *from = 0;
    *to = x;
    if (GC_general_register_disappearing_link(to, x) == GC_NO_MEMORY)
        dy_error("out of memory", 0);
}

D dy_weak_add(D list, D x) {
    WeakList *l = list == DY_EMPTY ? NULL : DY_PTR(list);
    intptr_t size = l ? l->size : 0;
    intptr_t used = l ? l->used : 0;
    if (used == size) {
        /* Full: the dead dropped, and if more than half is still alive, a
         * list twice the size, the links moved to it.  (An object read
         * here is on the stack, so it does not die while it moves.) */
        intptr_t alive = 0;
        for (intptr_t i = 0; i < used; i++) {
            void *y = l->objects[i];
            if (y) move_weak(y, &l->objects[i], &l->objects[alive++]);
        }
        if (!l || alive * 2 > size) {
            WeakList *bigger = make_weak_list(size ? size * 2 : 16);
            intptr_t moved = 0;
            for (intptr_t i = 0; i < alive; i++) {
                void *y = l->objects[i];
                if (y) move_weak(y, &l->objects[i], &bigger->objects[moved++]);
            }
            alive = moved;
            l = bigger;
        }
        used = alive;
    }
    l->objects[used] = DY_PTR(x);
    if (GC_general_register_disappearing_link(&l->objects[used], DY_PTR(x)) == GC_NO_MEMORY)
        dy_error("out of memory", 0);
    l->used = used + 1;
    return DY_OBJ(l);
}

D dy_weak_members(D list) {
    if (list == DY_EMPTY) return DY_EMPTY;
    WeakList *l = DY_PTR(list);
    D members = DY_EMPTY;
    for (intptr_t i = 0; i < l->used; i++) {
        D y = DY_OBJ(l->objects[i]);
        if (y) members = dy_pair(y, members);
    }
    return members;
}

void dy_weak_drop(D list) {
    if (list == DY_EMPTY) return;
    WeakList *l = DY_PTR(list);
    for (intptr_t i = 0; i < l->used; i++)
        GC_unregister_disappearing_link(&l->objects[i]);   /* if still there */
    l->used = 0;
}
#endif

void dy_init(void) {
#ifdef DY_MPS
    dy_gc_init();
#else
    /* Two threads marking, not one for each processor: more spend longer
     * waiting on each other than marking, even for a large heap shared by
     * many threads.  GC_MARKERS, if set, says otherwise. */
    if (!getenv("GC_MARKERS")) GC_set_markers_count(2);
    GC_INIT();
#ifdef DY_GC_STRESS
    const char *interval = getenv("DYLAN_GC_STRESS");
    if (interval && atol(interval) > 0) gc_stress_interval = atol(interval);
#endif
    GC_allow_register_threads();
#endif
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

/* The messages are the interactive implementation's, which reports
 * SBCL's conditions. */
_Noreturn void dy_float_trap(double result) {
    dy_error(isnan(result) ? "arithmetic error: floating-point-invalid-operation"
                           : "arithmetic error: floating-point-overflow", 0);
}

_Noreturn void dy_representation_error(const char *expected, D x) {
    char message[120];
    snprintf(message, sizeof message, "internal error: the compiled code expected %s", expected);
    dy_error(message, x);
}

D dy_pair(D head, D tail) {
    D fields[2] = { head, tail };
    return DY_OBJ(dy_alloc_init(&dy_wrapper_pair, sizeof(DyPair), fields, 2, 0));
}

D dy_make_vector(size_t size, D fill) {
    if (size > (size_t)DY_INT_MAX / sizeof(D)) dy_error("out of memory", 0);
    D length = DY_INT(size);
    return DY_OBJ(dy_alloc_init(&dy_wrapper_simple_object_vector,
                                sizeof(DyVector) + size * sizeof(D), &length, 1, fill));
}

/* ---------------------------------------------------------- Debugging */

void dy_put_char(FILE *out, uint32_t c) {
    /* UTF-8 has no encoding for a surrogate: it goes out as U+FFFD, the
     * replacement character, as in the interactive implementation. */
    if (c >= 0xD800 && c <= 0xDFFF) c = 0xFFFD;
    if (c < 0x80) fputc((int)c, out);
    else if (c < 0x800) { fputc(0xC0 | (c >> 6), out); fputc(0x80 | (c & 0x3F), out); }
    else if (c < 0x10000) {
        fputc(0xE0 | (c >> 12), out); fputc(0x80 | ((c >> 6) & 0x3F), out);
        fputc(0x80 | (c & 0x3F), out);
    } else {
        fputc(0xF0 | (c >> 18), out); fputc(0x80 | ((c >> 12) & 0x3F), out);
        fputc(0x80 | ((c >> 6) & 0x3F), out); fputc(0x80 | (c & 0x3F), out);
    }
}

bool dy_put_string(FILE *out, D s) {
    if (dy_kind_p(s, DY_KIND_BYTE_STRING)) {
        DyByteString *b = DY_PTR(s);
        intptr_t n = DY_INT_VALUE(b->size);
        for (intptr_t i = 0; i < n; i++) {
            unsigned char c = (unsigned char)b->data[i];
            if (c < 0x80) fputc(c, out);
            else dy_put_char(out, c);
        }
        return true;
    }
    if (dy_kind_p(s, DY_KIND_UNICODE_STRING)) {
        DyUnicodeString *u = DY_PTR(s);
        for (intptr_t i = 0; i < DY_INT_VALUE(u->size); i++) dy_put_char(out, u->data[i]);
        return true;
    }
    return false;
}

static void print_symbol_name(FILE *out, D name) {
    dy_put_string(out, name);
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
        /* As print does (14-printing.dyl) */
        uint32_t c = DY_CHAR_CODE(x);
        const char *name = NULL;
        switch (c) {
        case ' ': name = "space"; break;
        case '\n': name = "newline"; break;
        case 9: name = "tab"; break;
        case 13: name = "return"; break;
        case 12: name = "page"; break;
        case 0: name = "null"; break;
        case 127: name = "rubout"; break;
        case 8: name = "backspace"; break;
        case 27: name = "escape"; break;
        }
        fputs("#\\", out);
        if (name) fputs(name, out);
        else dy_put_char(out, c);
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
    case DY_KIND_BYTE_STRING:
    case DY_KIND_UNICODE_STRING: {
        /* As print does: " and \ escaped, the rest as they are */
        bool wide = w->kind == DY_KIND_UNICODE_STRING;
        intptr_t n = DY_INT_VALUE(((DyByteString *)DY_PTR(x))->size);
        fputc('"', out);
        for (intptr_t i = 0; i < n; i++) {
            uint32_t c = wide ? ((DyUnicodeString *)DY_PTR(x))->data[i]
                              : (unsigned char)((DyByteString *)DY_PTR(x))->data[i];
            if (c == '"' || c == '\\') fputc('\\', out);
            dy_put_char(out, c);
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
