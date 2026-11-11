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
    need(self, argc, 2, -1);    /* chapter 9: object1 object2 #rest objects */
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
    dy_reserve_values(argc);
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
    /* apply's own arguments between the function and the last are kept
     * first: the sequence hook's call may overwrite the argument area. */
    intptr_t before = argc - 2;
    D small[64];
    D *kept = before <= 64 ? small : dy_alloc_words((size_t)before);
    for (intptr_t i = 0; i < before; i++) kept[i] = ARG(i + 1);
    D last = ARG(argc - 1);
    DyVector *v = NULL;
    intptr_t count = 0;
    if (dy_kind_p(last, DY_KIND_SIMPLE_OBJECT_VECTOR)) {
        v = DY_PTR(last);
        count = DY_INT_VALUE(v->size);
    } else {
        if (last != DY_EMPTY && !dy_kind_p(last, DY_KIND_PAIR) && sequence_hook)
            /* "The last arg must be a sequence" (chapter 8) */
            last = DY_CALL(sequence_hook, last, 0, 0, 0, 1);
        for (D l = last; l != DY_EMPTY; l = dy_tail(l)) {
            /* An improper list: a <type-error> of its tail (design note 16) */
            if (!dy_kind_p(l, DY_KIND_PAIR)) dy_check_type(l, dy_wrapper_list.class);
            count++;
        }
    }
    intptr_t n = before + count;
    dy_reserve_arguments(n);
    Spread s = { { 0, 0, 0, 0 }, n };
    D l = last;
    for (intptr_t i = 0; i < n; i++) {
        D x;
        if (i < before) x = kept[i];
        else if (v) x = v->data[i - before];
        else { x = dy_head(l); l = dy_tail(l); }
        if (i < 4) s.a[i] = x;
        else *dy_argument_slot(i) = x;
    }
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
    /* The first argument is checked first, whatever order the C compiler
       evaluates a call's arguments in */
    DyClass *class1 = as_class(a0);
    DyClass *class2 = as_class(a1);
    return DY_BOOL(dy_wrapper_subtype(class1->instance_wrapper,
                                      class2->instance_wrapper));
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
    DyGeneric *g = DY_PTR(self);
    D *info = ((DyVector *)DY_PTR(g->info))->data;
    /* More than the required arguments only with #rest or #key */
    if (argc < g->required || (argc > g->required && info[1] == DY_FALSE && info[2] == DY_FALSE))
        dy_wrong_argument_count(self, argc);
    dy_no_applicable_method(self);
}

PRIMITIVE(make_generic_function) {
    need(self, argc, 4, 4);
    if (!DY_IS_INT(a0) || DY_INT_VALUE(a0) < 0) dy_error("make: bad required:", a0);
    if (a1 != DY_FALSE && !dy_kind_p(a1, DY_KIND_SYMBOL))
        dy_error("make: debug-name: must be a symbol", a1);
    DyGeneric *g = dy_alloc(&dy_wrapper_generic_function, sizeof(DyGeneric));
    g->entry = no_methods_entry;
    g->name = a1;
    g->methods = dy_make_vector(0, DY_FALSE);
    g->required = DY_INT_VALUE(a0);
    g->read_only = 0;
    /* #(required rest? keys), keys #f for no #key */
    D info = dy_make_vector(3, DY_FALSE);
    ((DyVector *)DY_PTR(info))->data[0] = a0;
    ((DyVector *)DY_PTR(info))->data[1] = a2;
    ((DyVector *)DY_PTR(info))->data[2] = a3;
    g->info = info;
    return DY_OBJ(g);
}

/* (seal class), and (seal generic-function), which in a static program
 * has nothing left to forbid.  A sealed class is read-only too. */
PRIMITIVE(seal) {
    need(self, argc, 1, 1);
    if (dy_kind_p(a0, DY_KIND_GENERIC)) return a0;
    DyClass *c = as_class(a0);
    c->sealed = 1;
    c->read_only = 1;
    return a0;
}

/* (make-read-only class-or-generic-function): no slot can be added to a
 * read-only class (chapter 11), the one change a static program can make
 * to it, and no method to a read-only generic function (chapter 10);
 * unlike seal, it hides nothing. */
PRIMITIVE(make_read_only) {
    need(self, argc, 1, 1);
    if (dy_kind_p(a0, DY_KIND_GENERIC)) ((DyGeneric *)DY_PTR(a0))->read_only = 1;
    else if (dy_instance_p(a0, &dy_wrapper_generic_function)) as_generic(a0);
    else as_class(a0)->read_only = 1;
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
    if (specs != DY_FALSE) return list_of_vector(specs);
    /* One for each required argument (chapter 10) */
    D first, result = DY_EMPTY;
    for (intptr_t i = dy_method_required(a0, &first) - 1; i >= 0; i--)
        result = dy_pair(i == 0 ? first : dy_wrapper_object.class, result);
    return result;
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
    if (specs == DY_FALSE) {
        /* Too few arguments make it inapplicable */
        D first;
        intptr_t required = dy_method_required(method, &first);
        return n >= required && (required == 0 || dy_specializer_matches(args[0], first));
    }
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
        args[i - first] = i < 4 ? in_registers[i] : *dy_argument_slot(i);
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

/* ---------------------- Generic functions made when the program runs */

/* A generic function make makes has no methods (no_methods_entry) until
 * a define-method of a variable that holds it adds one (chapter 6); then
 * it chooses its methods when it is called, as sorted-applicable-methods
 * sorts them.  It enters a method by its object, whose entry takes its
 * next method from dispatch_next, if dispatch_for is the object. */

static D made_dispatch(D self, D a0, D a1, D a2, D a3, intptr_t argc);

/* Run the first of METHODS, a list, with the rest as its next method. */
static D run_methods(D methods, D a0, D a1, D a2, D a3, intptr_t argc);

static D made_next_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    DY_TAIL_CALL_DIRECT(run_methods, ((DyFunction *)DY_PTR(self))->env[0],
                        a0, a1, a2, a3, argc);
}

/* The next method that runs the first of METHODS, a list, or #f.  (Its
 * one variable is stored after, not passed by address: the caller makes
 * a tail call.) */
static D made_next(D methods, D owner) {
    static const D one[1];
    if (methods == DY_EMPTY) return DY_FALSE;
    D next = dy_make_closure_env(made_next_entry, DY_FALSE, 1, one);
    ((DyFunction *)DY_PTR(next))->env[0] = methods;
    ((DyFunction *)DY_PTR(next))->owner = owner;
    return next;
}

static D run_methods(D methods, D a0, D a1, D a2, D a3, intptr_t argc) {
    D m = dy_head(methods);
    D next = made_next(dy_tail(methods), ((DyFunction *)DY_PTR(m))->owner);
    dy_thread->dispatch_for = m;            /* a slot's getter or setter */
    dy_thread->dispatch_next = next;        /* ignores them */
    DY_TAIL_CALL(m, a0, a1, a2, a3, argc);
}

static D made_dispatch(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    DyGeneric *g = DY_PTR(self);
    D *info = ((DyVector *)DY_PTR(g->info))->data;
    bool variable = info[1] != DY_FALSE || info[2] != DY_FALSE;
    intptr_t r = g->required;
    if (argc < r || (argc > r && !variable)) dy_wrong_argument_count(self, argc);
    D *args = dy_alloc_words(r + 1);
    for (intptr_t i = 0; i < r; i++)
        args[i] = i == 0 ? a0 : i == 1 ? a1 : i == 2 ? a2 : i == 3 ? a3 : *dy_argument_slot(i);
    DyVector *methods = DY_PTR(g->methods);
    intptr_t count = DY_INT_VALUE(methods->size), k = 0;
    D *found = dy_alloc_words(count + 1);
    for (intptr_t i = 0; i < count; i++)
        if (applicable(methods->data[i], args, r)) found[k++] = methods->data[i];
    if (k == 0) dy_no_applicable_method(self);
    for (intptr_t i = 1; i < k; i++) {      /* stable, most specific first */
        D m = found[i];
        intptr_t j = i;
        while (j > 0 && more_specific(m, found[j - 1], args, r)) {
            found[j] = found[j - 1];
            j--;
        }
        found[j] = m;
    }
    /* The keywords the applicable methods accept, as the compiler's
     * dispatch checks them (chapter 6) */
    if (variable) {
        bool keyed = info[2] != DY_FALSE, any = false;
        intptr_t n = 0;
        for (D l = info[2]; dy_kind_p(l, DY_KIND_PAIR); l = dy_tail(l)) n++;
        for (intptr_t i = 0; i < k; i++) {
            D *mi = ((DyVector *)DY_PTR(((DyFunction *)DY_PTR(found[i]))->info))->data;
            if (mi[1] != DY_FALSE) any = true;
            if (mi[2] != DY_FALSE) keyed = true;
            for (D l = mi[2]; dy_kind_p(l, DY_KIND_PAIR); l = dy_tail(l)) n++;
        }
        if (keyed && any) {
            dy_check_keywords(self, argc, r, a0, a1, a2, a3, NULL, 0);
        } else if (keyed) {
            D *accepted = dy_alloc_words(n + 1);
            intptr_t j = 0;
            for (D l = info[2]; dy_kind_p(l, DY_KIND_PAIR); l = dy_tail(l)) accepted[j++] = dy_head(l);
            for (intptr_t i = 0; i < k; i++) {
                D *mi = ((DyVector *)DY_PTR(((DyFunction *)DY_PTR(found[i]))->info))->data;
                for (D l = mi[2]; dy_kind_p(l, DY_KIND_PAIR); l = dy_tail(l)) accepted[j++] = dy_head(l);
            }
            dy_check_keywords(self, argc, r, a0, a1, a2, a3, accepted, (size_t)j);
        }
    }
    DY_TAIL_CALL_DIRECT(run_methods, dy_list_of(found, 0, k), a0, a1, a2, a3, argc);
}

/* Whether METHOD may be made a method of GENERIC: it is a method of no
 * generic function yet, or of GENERIC */
static bool free_or_of(D method, D generic) {
    D owner = ((DyFunction *)DY_PTR(method))->owner;
    return owner == 0 || owner == DY_FALSE || owner == generic;
}

/* A copy of METHOD, a method without free variables, of no generic
 * function */
static D method_copy(D method) {
    DyFunction *m = DY_PTR(method);
    D copy = dy_make_closure(m->entry, 0);
    DyFunction *c = DY_PTR(copy);
    c->specializers = m->specializers;
    c->info = m->info;
    c->name = m->name;
    return copy;
}

/* A define-method of the variable NAME, which holds GENERIC, another
 * generic function than its own: METHOD is added to it (chapter 6), and
 * kept in KEEP, if not NULL.  The errors first, then the change, with the
 * methods' lock held.  A method without free variables, which has no
 * KEEP, is one object each time its define-method runs: run for a second
 * generic function, it gives that one a copy, so that each has a method of
 * its own.  A closure is made afresh each time. */
void dy_add_method_when_run(D generic, D method, const char *name, D *keep) {
    if (!dy_kind_p(generic, DY_KIND_GENERIC))
        dy_error_naming("", name, " is defined both as a variable and as a generic function", 0);
    DyGeneric *g = DY_PTR(generic);
    if (g->entry != no_methods_entry && g->entry != made_dispatch)
        dy_error_naming("", name, " holds another generic function, which a compiled program cannot add a method to", 0);
    dy_check_addable(generic);
    const char *gname = g->name == DY_FALSE ? "an anonymous generic function" : dy_symbol_utf8(g->name);
    D *gi = ((DyVector *)DY_PTR(g->info))->data;
    D *mi = ((DyVector *)DY_PTR(((DyFunction *)DY_PTR(method))->info))->data;
    if (DY_INT_VALUE(gi[0]) != DY_INT_VALUE(mi[0])
        || (gi[1] != DY_FALSE || gi[2] != DY_FALSE) != (mi[1] != DY_FALSE || mi[2] != DY_FALSE))
        dy_error_naming("the parameter list of a method of ", gname,
                        " is not congruent with its generic function", 0);
    /* The keys it declares, each named by the method, unless it takes
     * #rest or #key alone */
    if (dy_kind_p(gi[2], DY_KIND_PAIR) && mi[1] == DY_FALSE && mi[2] != DY_EMPTY) {
        char missing[512] = "";
        size_t used = 0;
        for (D l = gi[2]; dy_kind_p(l, DY_KIND_PAIR); l = dy_tail(l)) {
            D m = mi[2];
            while (dy_kind_p(m, DY_KIND_PAIR) && dy_head(m) != dy_head(l)) m = dy_tail(m);
            if (!dy_kind_p(m, DY_KIND_PAIR) && used < sizeof missing - 1)
                used += (size_t)snprintf(missing + used, sizeof missing - used, "%s%s:",
                                         used ? " " : "", dy_symbol_utf8(dy_head(l)));
        }
        if (used) {
            char after[600];
            snprintf(after, sizeof after,
                     " does not accept the keys its generic function names: %s", missing);
            dy_error_naming("a method of ", gname, after, 0);
        }
    }
    /* It replaces the one with the same specializers (chapter 6). */
    D copy = 0;
    dy_lock_methods();
    while (!keep && !free_or_of(method, generic) && !copy) {
        dy_unlock_methods();
        copy = method_copy(method);
        dy_lock_methods();
    }
    if (!keep && !free_or_of(method, generic)) method = copy;
    DyVector *methods = DY_PTR(g->methods);
    D specs = ((DyFunction *)DY_PTR(method))->specializers;
    D replaced[1] = { 0 };
    intptr_t n = 0;
    for (intptr_t i = 0; i < DY_INT_VALUE(methods->size) && n == 0; i++) {
        D other = ((DyFunction *)DY_PTR(methods->data[i]))->specializers;
        if (specs == DY_FALSE || other == DY_FALSE) {
            if (specs == other) replaced[n++] = methods->data[i];
            continue;
        }
        DyVector *a = DY_PTR(specs), *b = DY_PTR(other);
        if (DY_INT_VALUE(a->size) != DY_INT_VALUE(b->size)) continue;
        intptr_t j = 0;
        while (j < DY_INT_VALUE(a->size) && same_specializer(a->data[j], b->data[j])) j++;
        if (j == DY_INT_VALUE(a->size)) replaced[n++] = methods->data[i];
    }
    ((DyFunction *)DY_PTR(method))->owner = generic;
    dy_add_method(generic, method, replaced, n);
    g->entry = made_dispatch;
    if (keep) *keep = method;
    dy_unlock_methods();
}

PRIMITIVE(find_method) {
    need(self, argc, 2, 2);
    DyVector *methods = DY_PTR(as_generic(a0)->methods);
    if (a1 != DY_EMPTY && !dy_kind_p(a1, DY_KIND_PAIR)) dy_error("not a list", a1);
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
