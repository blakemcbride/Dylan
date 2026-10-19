/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Built-in functions written in C: those the manual calls methods, which
 * programs cannot extend (id?, list, values, apply, ...), and the
 * reflective functions of chapters 10 and 11.  The rest of the library is
 * in Dylan (runtime/dylan).  The compiler knows these by the table in
 * compiler/backend/primitives.lisp.
 */

#include "dylan.h"
#include <stdlib.h>
#include <string.h>

#include "primitive.h"

/* id? (chapter 9): identity, except that numbers of one class and value
 * are id?. */
PRIMITIVE(id) {
    need(self, argc, 1, -1);
    for (intptr_t i = 0; i + 1 < argc; i++)
        if (!dy_identical(ARG(i), ARG(i + 1))) return DY_FALSE;
    return DY_TRUE;
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
/* The arguments of apply spread: the first four, the rest left in the
 * argument area.  (A function of its own, so that no local array of
 * apply's is live at its tail call.) */
typedef struct { D a[4]; intptr_t n; } Spread;

/* The library's function answering the elements of any sequence as a
 * list (%set-sequence-hook), for apply's last argument. */
static D sequence_hook;

PRIMITIVE(set_sequence_hook) {
    need(self, argc, 1, 1);
    sequence_hook = a0;
    return DY_FALSE;
}

static Spread spread_arguments(intptr_t argc, D a0, D a1, D a2, D a3) {
    D args[DY_MAX_ARGUMENTS];
    intptr_t n = 0;
    for (intptr_t i = 1; i + 1 < argc; i++) args[n++] = ARG(i);
    D last = ARG(argc - 1);
    if (dy_kind_p(last, DY_KIND_SIMPLE_OBJECT_VECTOR)) {
        DyVector *v = DY_PTR(last);
        if (n + DY_INT_VALUE(v->size) > DY_MAX_ARGUMENTS) dy_too_many_arguments();
        for (intptr_t i = 0; i < DY_INT_VALUE(v->size); i++) args[n++] = v->data[i];
        last = DY_EMPTY;
    } else if (last != DY_EMPTY && !dy_kind_p(last, DY_KIND_PAIR) && sequence_hook) {
        /* "The last arg must be a sequence" (chapter 8) */
        last = DY_CALL(sequence_hook, last, 0, 0, 0, 1);
    }
    for (D l = last; l != DY_EMPTY; l = dy_tail(l)) {
        if (!dy_kind_p(l, DY_KIND_PAIR)) dy_error("apply: not a list", ARG(argc - 1));
        if (n == DY_MAX_ARGUMENTS) dy_too_many_arguments();
        args[n++] = dy_head(l);
    }
    for (intptr_t i = 4; i < n; i++) dy_thread->args[i - 4] = args[i];
    Spread s = { { n > 0 ? args[0] : 0, n > 1 ? args[1] : 0,
                   n > 2 ? args[2] : 0, n > 3 ? args[3] : 0 }, n };
    return s;
}

PRIMITIVE(apply) {
    need(self, argc, 2, -1);
    D f = a0;
    Spread s = spread_arguments(argc, a0, a1, a2, a3);
    DY_TAIL_CALL(f, s.a[0], s.a[1], s.a[2], s.a[3], s.n);
}

/* --------------------------------------------- Classes and functions */

static D list_of_vector(D v) {
    DyVector *vec = DY_PTR(v);
    return dy_list_of(vec->data, 0, DY_INT_VALUE(vec->size));
}

static DyClass *as_class(D x) {
    if (!dy_kind_p(x, DY_KIND_CLASS)) dy_error("not a class", x);
    return DY_PTR(x);
}

static DyGeneric *as_generic(D x) {
    if (!dy_kind_p(x, DY_KIND_GENERIC)) dy_error("not a generic function", x);
    return DY_PTR(x);
}

PRIMITIVE(object_class) { need(self, argc, 1, 1); return dy_object_class(a0); }

PRIMITIVE(instance_p) {
    need(self, argc, 2, 2);
    return DY_BOOL(dy_specializer_matches(a0, a1));
}

PRIMITIVE(subclass_p) {
    need(self, argc, 2, 2);
    return DY_BOOL(dy_wrapper_subtype(as_class(a0)->instance_wrapper,
                                      as_class(a1)->instance_wrapper));
}

PRIMITIVE(singleton) {
    need(self, argc, 1, 1);
    DySingleton *s = dy_alloc(&dy_wrapper_singleton, sizeof(DySingleton));
    s->object = a0;
    return DY_OBJ(s);
}

/* Chapter 11: on a class seal has sealed, these answer an empty
 * sequence. */
PRIMITIVE(all_superclasses) {
    need(self, argc, 1, 1);
    DyClass *c = as_class(a0);
    return c->sealed ? DY_EMPTY : list_of_vector(c->precedence_list);
}

PRIMITIVE(direct_superclasses) {
    need(self, argc, 1, 1);
    DyClass *c = as_class(a0);
    return c->sealed ? DY_EMPTY : list_of_vector(c->direct_superclasses);
}

PRIMITIVE(direct_subclasses) {
    need(self, argc, 1, 1);
    return list_of_vector(as_class(a0)->direct_subclasses);
}

/* Making instances, and the operations the static program makes no-ops */

PRIMITIVE(make) {
    need(self, argc, 1, -1);
    DyClass *c = as_class(a0);
    if (!c->maker) dy_error("cannot make an instance of this class", a0);
    DY_TAIL_CALL_DIRECT(c->maker, a0, a0, a1, a2, a3, argc);
}

/* (%make-generic-function required name): a generic function made at run
 * time, which has no methods. */
static D no_methods_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    (void)a0; (void)a1; (void)a2; (void)a3;
    if (argc != ((DyGeneric *)DY_PTR(self))->required) dy_wrong_argument_count(self, argc);
    dy_no_applicable_method(self);
}

PRIMITIVE(make_generic_function) {
    need(self, argc, 2, 2);
    if (!DY_IS_INT(a0) || DY_INT_VALUE(a0) < 0) dy_error("make: bad required:", a0);
    if (a1 != DY_FALSE && !dy_kind_p(a1, DY_KIND_SYMBOL))
        dy_error("make: debug-name: must be a symbol", a1);
    DyGeneric *g = dy_alloc(&dy_wrapper_generic_function, sizeof(DyGeneric));
    g->entry = no_methods_entry;
    g->name = a1;
    g->methods = dy_make_vector(0, DY_FALSE);
    g->required = DY_INT_VALUE(a0);
    D info = dy_make_vector(3, DY_FALSE);
    ((DyVector *)DY_PTR(info))->data[0] = a0;
    g->info = info;
    return DY_OBJ(g);
}

/* (seal class), and (make-read-only generic-function), which in a
 * static program has nothing left to forbid. */
PRIMITIVE(seal) {
    need(self, argc, 1, 1);
    if (dy_kind_p(a0, DY_KIND_GENERIC)) return a0;
    as_class(a0)->sealed = 1;
    return a0;
}

PRIMITIVE(generic_function_methods) {
    need(self, argc, 1, 1);
    return list_of_vector(as_generic(a0)->methods);
}

PRIMITIVE(method_specializers) {
    need(self, argc, 1, 1);
    if (!dy_kind_p(a0, DY_KIND_FUNCTION)) dy_error("not a method", a0);
    D specs = ((DyFunction *)DY_PTR(a0))->specializers;
    return specs == DY_FALSE ? DY_EMPTY : list_of_vector(specs);
}

/* Does METHOD -- or, of a generic function, one of its methods -- apply
 * to the N arguments ARGS? */
static bool applicable(D method, const D *args, intptr_t n) {
    if (dy_kind_p(method, DY_KIND_GENERIC)) {
        DyVector *methods = DY_PTR(((DyGeneric *)DY_PTR(method))->methods);
        for (intptr_t i = 0; i < DY_INT_VALUE(methods->size); i++)
            if (applicable(methods->data[i], args, n)) return true;
        return false;
    }
    if (!dy_kind_p(method, DY_KIND_FUNCTION)) dy_error("not a function", method);
    D specs = ((DyFunction *)DY_PTR(method))->specializers;
    if (specs == DY_FALSE) return true;
    DyVector *v = DY_PTR(specs);
    if (DY_INT_VALUE(v->size) > n) return false;
    for (intptr_t i = 0; i < DY_INT_VALUE(v->size); i++)
        if (!dy_specializer_matches(args[i], v->data[i])) return false;
    return true;
}

static D *arguments_after(intptr_t first, intptr_t argc, D a0, D a1, D a2, D a3,
                          intptr_t *n) {
    D in_registers[4] = { a0, a1, a2, a3 };
    *n = argc - first;
    D *args = dy_alloc_words(*n + 1);
    for (intptr_t i = first; i < argc; i++)
        args[i - first] = i < 4 ? in_registers[i] : dy_thread->args[i - 4];
    return args;
}

PRIMITIVE(applicable_method_p) {
    need(self, argc, 1, -1);
    intptr_t n;
    D *args = arguments_after(1, argc, a0, a1, a2, a3, &n);
    return DY_BOOL(applicable(a0, args, n));
}

/* Chapter 10: specializer lists compared lexicographically, a singleton
 * before any class, and a class before its superclasses -- here, as in
 * CLOS, the one earlier in the precedence list of the argument's class. */
static int rank(D spec, D arg) {
    if (dy_kind_p(spec, DY_KIND_SINGLETON)) return -1;
    DyVector *cpl = DY_PTR(((DyClass *)DY_PTR(dy_object_class(arg)))->precedence_list);
    for (intptr_t i = 0; i < DY_INT_VALUE(cpl->size); i++)
        if (cpl->data[i] == spec) return (int)i;
    return 1 << 30;
}

static bool more_specific(D m1, D m2, const D *args, intptr_t n) {
    D s1 = ((DyFunction *)DY_PTR(m1))->specializers;
    D s2 = ((DyFunction *)DY_PTR(m2))->specializers;
    if (s1 == DY_FALSE || s2 == DY_FALSE) return false;
    DyVector *v1 = DY_PTR(s1), *v2 = DY_PTR(s2);
    for (intptr_t i = 0; i < n && i < DY_INT_VALUE(v1->size); i++) {
        int r1 = rank(v1->data[i], args[i]), r2 = rank(v2->data[i], args[i]);
        if (r1 != r2) return r1 < r2;
    }
    return false;
}

PRIMITIVE(sorted_applicable_methods) {
    need(self, argc, 1, -1);
    intptr_t n;
    D *args = arguments_after(1, argc, a0, a1, a2, a3, &n);
    DyVector *methods = DY_PTR(as_generic(a0)->methods);
    intptr_t count = DY_INT_VALUE(methods->size), k = 0;
    D *found = dy_alloc_words(count + 1);
    for (intptr_t i = 0; i < count; i++)
        if (applicable(methods->data[i], args, n)) found[k++] = methods->data[i];
    /* Insertion sort: stable, most specific first. */
    for (intptr_t i = 1; i < k; i++) {
        D m = found[i];
        intptr_t j = i;
        while (j > 0 && more_specific(m, found[j - 1], args, n)) {
            found[j] = found[j - 1];
            j--;
        }
        found[j] = m;
    }
    return dy_list_of(found, 0, k);
}

static bool same_specializer(D x, D y) {
    if (x == y) return true;
    return dy_kind_p(x, DY_KIND_SINGLETON) && dy_kind_p(y, DY_KIND_SINGLETON)
        && dy_identical(((DySingleton *)DY_PTR(x))->object,
                        ((DySingleton *)DY_PTR(y))->object);
}

PRIMITIVE(find_method) {
    need(self, argc, 2, 2);
    DyVector *methods = DY_PTR(as_generic(a0)->methods);
    for (intptr_t i = 0; i < DY_INT_VALUE(methods->size); i++) {
        D specs = ((DyFunction *)DY_PTR(methods->data[i]))->specializers;
        if (specs == DY_FALSE) continue;
        DyVector *v = DY_PTR(specs);
        D l = a1;
        intptr_t j = 0;
        for (; j < DY_INT_VALUE(v->size) && dy_kind_p(l, DY_KIND_PAIR); j++, l = dy_tail(l))
            if (!same_specializer(v->data[j], dy_head(l))) break;
        if (j == DY_INT_VALUE(v->size) && l == DY_EMPTY) return methods->data[i];
    }
    return DY_FALSE;
}
