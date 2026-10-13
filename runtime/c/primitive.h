/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Writing built-in functions in C.  PRIMITIVE(name) defines the DyFunction
 * dy_prim_name and opens the body of its uniform entry, in which ARG(i) is
 * argument i.  The compiler knows them by compiler/backend/primitives.lisp.
 */

#ifndef DYLAN_PRIMITIVE_H
#define DYLAN_PRIMITIVE_H

#include "dylan.h"

#define ARG(i) ((i) == 0 ? a0 : (i) == 1 ? a1 : (i) == 2 ? a2 : (i) == 3 ? a3 \
                : dy_thread->args[(i) - 4])

#define U __attribute__((unused))
#define PRIMITIVE(c_name)                                                   \
    static D c_name##_entry(D, D, D, D, D, intptr_t);                       \
    DyFunction dy_prim_##c_name = { &dy_wrapper_method, c_name##_entry,     \
                                    DY_FALSE };                             \
    static D c_name##_entry(U D self, U D a0, U D a1, U D a2, U D a3,      \
                            U intptr_t argc)

static inline void need(D self, intptr_t argc, intptr_t min, intptr_t max) {
    if (argc < min || (max >= 0 && argc > max)) dy_wrong_argument_count(self, argc);
}

#endif
