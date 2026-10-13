/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Provisional built-in functions.
 *
 * Until the library is written (CompilerPlan.md, Phase 8), these few
 * functions of the DYLAN module are written here in C, so that programs
 * can run and the code generator be tested.  The compiler knows them by
 * the table in compiler/backend/primitives.lisp.  Each is a DyFunction
 * with the uniform entry.  Arithmetic and comparison go through number.c;
 * = and id? on objects other than numbers compare identity.
 */

#include "dylan.h"
#include <stdlib.h>
#include <string.h>

#define ARG(i) ((i) == 0 ? a0 : (i) == 1 ? a1 : (i) == 2 ? a2 : (i) == 3 ? a3 \
                : dy_thread->args[(i) - 4])

#define U __attribute__((unused))
#define PRIMITIVE(c_name)                                                   \
    static D c_name##_entry(D, D, D, D, D, intptr_t);                       \
    DyFunction dy_prim_##c_name = { &dy_wrapper_method, c_name##_entry };   \
    static D c_name##_entry(U D self, U D a0, U D a1, U D a2, U D a3,      \
                            U intptr_t argc)

static void need(D self, intptr_t argc, intptr_t min, intptr_t max) {
    if (argc < min || (max >= 0 && argc > max)) dy_wrong_argument_count(self, argc);
}

/* ---------------------------------------------------------- Arithmetic */

PRIMITIVE(add) {
    D sum = DY_INT(0);
    for (intptr_t i = 0; i < argc; i++) sum = dy_add(sum, ARG(i));
    return sum;
}

PRIMITIVE(subtract) {
    need(self, argc, 1, -1);
    if (argc == 1) return dy_negate(a0);
    D r = a0;
    for (intptr_t i = 1; i < argc; i++) r = dy_sub(r, ARG(i));
    return r;
}

PRIMITIVE(multiply) {
    D product = DY_INT(1);
    for (intptr_t i = 0; i < argc; i++) product = dy_mul(product, ARG(i));
    return product;
}

PRIMITIVE(divide) {
    need(self, argc, 1, -1);
    if (argc == 1) return dy_div(DY_INT(1), a0);
    D r = a0;
    for (intptr_t i = 1; i < argc; i++) r = dy_div(r, ARG(i));
    return r;
}

/* = : numbers by value, anything else by identity (for now) */
static bool equal(D x, D y) {
    if (dy_number_p(x) && dy_number_p(y)) return dy_number_equal(x, y);
    return x == y;
}

/* id? : identity; numbers of the same class and value are id? */
static bool identical(D x, D y) {
    if (x == y) return true;
    if (dy_number_p(x) && dy_number_p(y) && dy_wrapper_of(x) == dy_wrapper_of(y))
        return dy_compare(x, y) == 0;
    return false;
}

#define CHAINED(c_name, test)                                           \
    PRIMITIVE(c_name) {                                                 \
        need(self, argc, 1, -1);                                        \
        for (intptr_t i = 0; i + 1 < argc; i++) {                       \
            D x = ARG(i), y = ARG(i + 1);                               \
            if (!(test)) return DY_FALSE;                               \
        }                                                               \
        return DY_TRUE;                                                 \
    }

CHAINED(less, dy_number_less(x, y))
CHAINED(greater, dy_number_less(y, x))
CHAINED(less_equal, (dy_compare(x, y) == -1 || dy_compare(x, y) == 0))
CHAINED(greater_equal, (dy_compare(x, y) == 1 || dy_compare(x, y) == 0))
CHAINED(equal, equal(x, y))
CHAINED(id, identical(x, y))

PRIMITIVE(not_equal) {
    need(self, argc, 2, 2);
    return DY_BOOL(!equal(a0, a1));
}

PRIMITIVE(zero_p) { need(self, argc, 1, 1); return DY_BOOL(dy_number_equal(a0, DY_INT(0))); }
PRIMITIVE(positive_p) { need(self, argc, 1, 1); return DY_BOOL(dy_number_less(DY_INT(0), a0)); }
PRIMITIVE(negative_p) { need(self, argc, 1, 1); return DY_BOOL(dy_number_less(a0, DY_INT(0))); }

PRIMITIVE(even_p) {
    need(self, argc, 1, 1);
    if (!DY_IS_INT(a0)) dy_error("not an integer", a0);
    return DY_BOOL((DY_INT_VALUE(a0) & 1) == 0);
}

PRIMITIVE(odd_p) {
    need(self, argc, 1, 1);
    if (!DY_IS_INT(a0)) dy_error("not an integer", a0);
    return DY_BOOL((DY_INT_VALUE(a0) & 1) != 0);
}

PRIMITIVE(abs) {
    need(self, argc, 1, 1);
    return dy_number_less(a0, DY_INT(0)) ? dy_negate(a0) : a0;
}

PRIMITIVE(max) {
    need(self, argc, 1, -1);
    D r = a0;
    for (intptr_t i = 1; i < argc; i++) if (dy_number_less(r, ARG(i))) r = ARG(i);
    return r;
}

PRIMITIVE(min) {
    need(self, argc, 1, -1);
    D r = a0;
    for (intptr_t i = 1; i < argc; i++) if (dy_number_less(ARG(i), r)) r = ARG(i);
    return r;
}

/* ------------------------------------------------------------- Objects */

PRIMITIVE(not) { need(self, argc, 1, 1); return DY_BOOL(a0 == DY_FALSE); }
PRIMITIVE(identity) { need(self, argc, 1, 1); return a0; }

PRIMITIVE(list) {
    return dy_collect_arguments(argc, 0, a0, a1, a2, a3);
}

PRIMITIVE(cons) { need(self, argc, 2, 2); return dy_pair(a0, a1); }

PRIMITIVE(car) {
    need(self, argc, 1, 1);
    if (a0 == DY_EMPTY) return DY_EMPTY;
    if (!dy_kind_p(a0, DY_KIND_PAIR)) dy_error("not a list", a0);
    return dy_head(a0);
}

PRIMITIVE(cdr) {
    need(self, argc, 1, 1);
    if (a0 == DY_EMPTY) return DY_EMPTY;
    if (!dy_kind_p(a0, DY_KIND_PAIR)) dy_error("not a list", a0);
    return dy_tail(a0);
}

PRIMITIVE(vector) {
    D v = dy_make_vector(argc, DY_FALSE);
    for (intptr_t i = 0; i < argc; i++) ((DyVector *)DY_PTR(v))->data[i] = ARG(i);
    return v;
}

/* ------------------------------------------------------------ Calling */

PRIMITIVE(values) {
    DyThread *t = dy_thread;
    for (intptr_t i = 0; i < argc; i++) t->mv[i] = ARG(i);
    t->mv_count = argc;
    return argc ? a0 : DY_FALSE;
}

/* (apply f arg ... list) calls f with the args and the list's elements,
 * as a tail call. */
PRIMITIVE(apply) {
    need(self, argc, 2, -1);
    D f = a0;
    D args[DY_MAX_ARGUMENTS];
    intptr_t n = 0;
    for (intptr_t i = 1; i + 1 < argc; i++) args[n++] = ARG(i);
    for (D l = ARG(argc - 1); l != DY_EMPTY; l = dy_tail(l)) {
        if (!dy_kind_p(l, DY_KIND_PAIR)) dy_error("apply: not a list", ARG(argc - 1));
        if (n == DY_MAX_ARGUMENTS) dy_too_many_arguments();
        args[n++] = dy_head(l);
    }
    for (intptr_t i = 4; i < n; i++) dy_thread->args[i - 4] = args[i];
    D b0 = n > 0 ? args[0] : 0, b1 = n > 1 ? args[1] : 0,
      b2 = n > 2 ? args[2] : 0, b3 = n > 3 ? args[3] : 0;
    DY_TAIL_CALL(f, b0, b1, b2, b3, n);
}

/* ------------------------------------------------------------- Output */

PRIMITIVE(print) {
    need(self, argc, 1, 1);
    dy_debug_print(a0);
    putchar('\n');
    return a0;
}

/* (error string arg ...): until conditions (Phase 6), report and stop. */
PRIMITIVE(error) {
    need(self, argc, 1, -1);
    fflush(stdout);
    fputs("Dylan error: ", stderr);
    if (dy_kind_p(a0, DY_KIND_BYTE_STRING)) {
        DyByteString *s = DY_PTR(a0);
        fwrite(s->data, 1, DY_INT_VALUE(s->size), stderr);
    } else {
        dy_debug_write(stderr, a0);
    }
    for (intptr_t i = 1; i < argc; i++) {
        fputc(' ', stderr);
        dy_debug_write(stderr, ARG(i));
    }
    fputc('\n', stderr);
    exit(1);
}
