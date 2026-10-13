/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Functions: closures, arguments, per-thread state, and the errors of
 * calling.
 */

#include "dylan.h"
#include <string.h>

_Thread_local DyThread *dy_thread;

DyHeader dy_unbound_object = { &dy_wrapper_object };
DyHeader dy_unsupplied_object = { &dy_wrapper_object };
DyHeader dy_tail_call_object = { &dy_wrapper_object };

void dy_init_thread(void) {
    if (dy_thread) return;
    dy_thread = GC_MALLOC_UNCOLLECTABLE(sizeof(DyThread));
    if (!dy_thread) dy_error("out of memory", 0);
    memset(dy_thread, 0, sizeof(DyThread));
    dy_thread->mv_count = 1;
}

D dy_make_closure(DyEntry entry, size_t env_size) {
    DyFunction *f = dy_alloc(&dy_wrapper_method,
                             sizeof(DyFunction) + env_size * sizeof(D));
    f->entry = entry;
    return DY_OBJ(f);
}

D dy_collect_arguments(intptr_t argc, intptr_t start, D a0, D a1, D a2, D a3) {
    D in_registers[4] = { a0, a1, a2, a3 };
    D list = DY_EMPTY;
    for (intptr_t i = argc - 1; i >= start; i--)
        list = dy_pair(i < 4 ? in_registers[i] : dy_thread->args[i - 4], list);
    return list;
}

D dy_list_of(const D *values, intptr_t start, intptr_t end) {
    D list = DY_EMPTY;
    for (intptr_t i = end - 1; i >= start; i--) list = dy_pair(values[i], list);
    return list;
}

void dy_check_type(D value, D type) {
    if (!dy_kind_p(type, DY_KIND_CLASS))
        dy_error("only classes are supported as types so far", type);
    if (!dy_instance_p(value, ((DyClass *)DY_PTR(type))->instance_wrapper))
        dy_error("type error: the value is not of the declared type", value);
}

_Noreturn void dy_not_a_function(D f) { dy_error("not a function", f); }

_Noreturn void dy_wrong_argument_count(D f, intptr_t argc) {
    (void)f;
    char message[80];
    snprintf(message, sizeof message, "wrong number of arguments (%ld)", (long)argc);
    dy_error(message, 0);
}

_Noreturn void dy_unbound_variable(const char *name) {
    char message[256];
    snprintf(message, sizeof message, "the variable %s has no value", name);
    dy_error(message, 0);
}

_Noreturn void dy_odd_keyword_arguments(D f) {
    (void)f;
    dy_error("the arguments after the required ones are not keyword/value pairs", 0);
}

_Noreturn void dy_unknown_keyword(D f, D keyword) {
    (void)f;
    dy_error("keyword argument not accepted", keyword);
}

_Noreturn void dy_too_many_arguments(void) {
    dy_error("too many arguments or values (the limit is 1024)", 0);
}

/* ---------------------------------------------------------- Backquote */

D dy_append2(D list, D tail) {
    if (list == DY_EMPTY) return tail;
    D head = dy_pair(dy_head(list), DY_EMPTY), last = head;
    for (D l = dy_tail(list); dy_kind_p(l, DY_KIND_PAIR); l = dy_tail(l)) {
        D cell = dy_pair(dy_head(l), DY_EMPTY);
        ((DyPair *)DY_PTR(last))->tail = cell;
        last = cell;
    }
    ((DyPair *)DY_PTR(last))->tail = tail;
    return head;
}

D dy_list_to_vector(D list) {
    size_t n = 0;
    for (D l = list; l != DY_EMPTY; l = dy_tail(l)) n++;
    D v = dy_make_vector(n, DY_FALSE);
    size_t i = 0;
    for (D l = list; l != DY_EMPTY; l = dy_tail(l))
        ((DyVector *)DY_PTR(v))->data[i++] = dy_head(l);
    return v;
}
