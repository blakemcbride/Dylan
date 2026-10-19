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
    dy_thread = dy_alloc_root(sizeof(DyThread));
    memset(dy_thread, 0, sizeof(DyThread));
    dy_thread->mv_count = 1;
#ifdef DY_MPS
    dy_mps_thread_start();
#endif
    size_t n = dy_thread_variable_count;
    dy_thread->thread_variables = dy_alloc_root((n ? n : 1) * sizeof(D));
    for (size_t i = 0; i < n; i++)
        dy_thread->thread_variables[i] = dy_thread_variable_defaults[i];
}

void dy_end_thread(void) {
#ifdef DY_MPS
    dy_mps_thread_end();
#endif
    dy_free_root(dy_thread->thread_variables);
    dy_free_root(dy_thread);
    dy_thread = NULL;
}

D dy_make_closure(DyEntry entry, size_t env_size) {
#ifdef DY_MPS
    DyFunction *f = dy_mps_alloc(DY_POOL_EXACT, &dy_wrapper_method,
                                 sizeof(DyFunction) + env_size * sizeof(D),
                                 offsetof(DyFunction, env_count), (D)env_size);
#else
    DyFunction *f = dy_alloc(&dy_wrapper_method,
                             sizeof(DyFunction) + env_size * sizeof(D));
#endif
    f->entry = entry;
    f->specializers = DY_FALSE;
    f->info = DY_FALSE;
    f->owner = DY_FALSE;
    f->name = NULL;
    return DY_OBJ(f);
}

#ifdef DY_MPS
_Static_assert(offsetof(DyFunction, env) == 7 * sizeof(D), "DyFunction's layout");
#else
_Static_assert(offsetof(DyFunction, env) == 6 * sizeof(D), "DyFunction's layout");
#endif

D dy_make_closure_env(DyEntry entry, D info, size_t env_size, const D *env) {
#ifdef DY_MPS
    /* entry, specializers, info, owner, name, env_count, env */
    size_t n = 6 + env_size;
#else
    size_t n = 5 + env_size;
#endif
    D fields[6 + 64];
    D *f = n <= sizeof fields / sizeof fields[0] ? fields : dy_alloc_words(n);
    f[0] = (D)entry;
    f[1] = DY_FALSE;
    f[2] = info;
    f[3] = DY_FALSE;
    f[4] = 0;
#ifdef DY_MPS
    f[5] = (D)env_size;
#endif
    for (size_t i = 0; i < env_size; i++) f[n - env_size + i] = env[i];
    return DY_OBJ(dy_alloc_init(&dy_wrapper_method, sizeof(DyFunction) + env_size * sizeof(D),
                                f, n, 0));
}

void dy_name_method(D f, const char *name) {
    if (!dy_kind_p(f, DY_KIND_FUNCTION)) return;
    DyFunction *m = DY_PTR(f);
    if ((m->owner == 0 || m->owner == DY_FALSE) && !m->name) m->name = name;
}

D dy_collect_arguments(intptr_t argc, intptr_t start, D a0, D a1, D a2, D a3) {
    D in_registers[4] = { a0, a1, a2, a3 };
    D list = DY_EMPTY;
    for (intptr_t i = argc - 1; i >= start; i--)
        list = dy_pair(i < 4 ? in_registers[i] : dy_thread->args[i - 4], list);
    return list;
}

D dy_collect_arguments_into(DyPair *pairs, intptr_t argc, intptr_t start,
                            D a0, D a1, D a2, D a3) {
    D in_registers[4] = { a0, a1, a2, a3 };
    D list = DY_EMPTY;
    for (intptr_t i = argc - 1; i >= start; i--) {
        DyPair *p = &pairs[i - start];
        p->wrapper = &dy_wrapper_pair;
        p->head = i < 4 ? in_registers[i] : dy_thread->args[i - 4];
        p->tail = list;
        list = DY_OBJ(p);
    }
    return list;
}

D dy_list_of(const D *values, intptr_t start, intptr_t end) {
    D list = DY_EMPTY;
    for (intptr_t i = end - 1; i >= start; i--) list = dy_pair(values[i], list);
    return list;
}

_Noreturn void dy_not_a_function(D f) { dy_error("not a function", f); }

_Noreturn void dy_wrong_argument_count(D f, intptr_t argc) {
    (void)f;
    char message[80];
    snprintf(message, sizeof message, "wrong number of arguments (%" PRIdPTR ")", argc);
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
