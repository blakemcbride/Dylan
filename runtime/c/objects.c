/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Support for the object system (chapters 10 and 11): specializers,
 * keyword checking by generic functions, next methods, instances, and
 * the errors of dispatch and slots.  The dispatch itself is compiled: see
 * compiler/backend/objects.lisp.
 */

#include "dylan.h"
#include <stdlib.h>
#include <string.h>

bool dy_identical(D x, D y) {
    if (x == y) return true;
    if (dy_number_p(x) && dy_number_p(y) && dy_wrapper_of(x) == dy_wrapper_of(y))
        return dy_compare(x, y) == 0;
    return false;
}

bool dy_specializer_matches(D x, D spec) {
    if (dy_kind_p(spec, DY_KIND_CLASS))
        return dy_instance_p(x, ((DyClass *)DY_PTR(spec))->instance_wrapper);
    if (dy_kind_p(spec, DY_KIND_SINGLETON))
        return dy_identical(x, ((DySingleton *)DY_PTR(spec))->object);
    dy_error("not a class or singleton", spec);
}

static const char *generic_name(D generic) {
    D symbol = ((DyGeneric *)DY_PTR(generic))->name;
    if (symbol == DY_FALSE) return "an anonymous generic function";
    DySymbol *name = DY_PTR(symbol);
    return ((DyByteString *)DY_PTR(name->name))->data;
}

_Noreturn void dy_no_applicable_method(D generic) {
    char message[256];
    snprintf(message, sizeof message, "no applicable method for %s",
             generic_name(generic));
    dy_error(message, 0);
}

_Noreturn void dy_unbound_slot(const char *getter) {
    char message[256];
    snprintf(message, sizeof message, "the slot %s is not initialized", getter);
    dy_error(message, 0);
}

_Noreturn void dy_not_a_slot(const char *getter, D instance) {
    char message[256];
    snprintf(message, sizeof message, "%s is not a slot of the object", getter);
    dy_error(message, instance);
}

void dy_check_keywords(D f, intptr_t argc, intptr_t start, D a0, D a1, D a2,
                       D a3, const D *accepted, size_t n) {
    D in_registers[4] = { a0, a1, a2, a3 };
    if ((argc - start) % 2 != 0) dy_odd_keyword_arguments(f);
    for (intptr_t i = start; i < argc; i += 2) {
        D key = i < 4 ? in_registers[i] : dy_thread->args[i - 4];
        if (!dy_kind_p(key, DY_KIND_KEYWORD)) dy_odd_keyword_arguments(f);
        if (accepted) {
            size_t j = 0;
            while (j < n && accepted[j] != key) j++;
            if (j == n) dy_unknown_keyword(f, key);
        }
    }
}

/* ------------------------------------------------------- Next methods */

/* env[0] is the next method, env[1] the argument count, env[2..] the
 * arguments. */
static D captured_next_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    DyFunction *c = DY_PTR(self);
    D next = c->env[0];
    if (argc == 0) {
        argc = DY_INT_VALUE(c->env[1]);
        for (intptr_t i = 4; i < argc; i++) dy_thread->args[i - 4] = c->env[2 + i];
        a0 = argc > 0 ? c->env[2] : 0;
        a1 = argc > 1 ? c->env[3] : 0;
        a2 = argc > 2 ? c->env[4] : 0;
        a3 = argc > 3 ? c->env[5] : 0;
    }
    DY_TAIL_CALL(next, a0, a1, a2, a3, argc);
}

D dy_capture_next(D next, intptr_t argc, D a0, D a1, D a2, D a3) {
    size_t n = argc < 4 ? 4 : (size_t)argc;
    D c = dy_make_closure(captured_next_entry, 2 + n);
    DyFunction *f = DY_PTR(c);
    f->env[0] = next;
    f->env[1] = DY_INT(argc);
    f->env[2] = a0; f->env[3] = a1; f->env[4] = a2; f->env[5] = a3;
    for (intptr_t i = 4; i < argc; i++) f->env[2 + i] = dy_thread->args[i - 4];
    return c;
}

/* ---------------------------------------------------------- Instances */

D dy_make_instance(DyWrapper *wrapper) {
    DyInstance *x = dy_alloc(wrapper, sizeof(DyInstance) + wrapper->size * sizeof(D));
    for (uint32_t i = 0; i < wrapper->size; i++) x->slots[i] = DY_UNBOUND;
    return DY_OBJ(x);
}

D dy_find_keyword(D pairs, D keyword) {
    for (D l = pairs; l != DY_EMPTY; l = dy_tail(dy_tail(l)))
        if (dy_head(l) == keyword) return dy_head(dy_tail(l));
    return DY_UNSUPPLIED;
}

D dy_call_list(D f, D first, D list) {
    D args[DY_MAX_ARGUMENTS];
    intptr_t n = 0;
    args[n++] = first;
    for (D l = list; l != DY_EMPTY; l = dy_tail(l)) {
        if (n == DY_MAX_ARGUMENTS) dy_too_many_arguments();
        args[n++] = dy_head(l);
    }
    for (intptr_t i = 4; i < n; i++) dy_thread->args[i - 4] = args[i];
    dy_thread->next_method = DY_FALSE;
    return DY_CALL(f, args[0], n > 1 ? args[1] : 0, n > 2 ? args[2] : 0,
                   n > 3 ? args[3] : 0, n);
}

static D default_initialize_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    (void)self; (void)a0; (void)a1; (void)a2; (void)a3; (void)argc;
    return DY_FALSE;
}

DyFunction dy_default_initialize = { &dy_wrapper_method, default_initialize_entry,
                                     DY_FALSE, DY_FALSE, DY_FALSE, NULL };
