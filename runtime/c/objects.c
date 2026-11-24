/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Support for the object system (chapters 10 and 11): specializers,
 * keyword checking by generic functions, next methods, instances, and
 * the errors of dispatch and slots.  The dispatch itself is compiled: see
 * compiler/backend/objects.lisp.
 */

#include "dylan.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* id? (chapter 9): objects no program can tell apart.  Numbers of one
 * class and value are; but not a float and its negative zero, which are =
 * yet print differently. */
bool dy_identical(D x, D y) {
    if (x == y) return true;
    if (dy_number_p(x) && dy_number_p(y) && dy_wrapper_of(x) == dy_wrapper_of(y)) {
        if (dy_compare(x, y) != 0) return false;
        if (dy_kind_p(x, DY_KIND_SINGLE_FLOAT) || dy_kind_p(x, DY_KIND_DOUBLE_FLOAT)
            || dy_kind_p(x, DY_KIND_EXTENDED_FLOAT))
            return signbit(dy_float_value(x)) == signbit(dy_float_value(y));
        return true;
    }
    return false;
}

bool dy_specializer_matches(D x, D spec) {
    if (dy_kind_p(spec, DY_KIND_CLASS))
        return dy_instance_p(x, ((DyClass *)DY_PTR(spec))->instance_wrapper);
    if (dy_kind_p(spec, DY_KIND_SINGLETON))
        return dy_identical(x, ((DySingleton *)DY_PTR(spec))->object);
    dy_error("not a class or singleton", spec);
}

void dy_check_specializer(D spec) {
    if (!dy_kind_p(spec, DY_KIND_CLASS) && !dy_kind_p(spec, DY_KIND_SINGLETON))
        dy_error("not a class or singleton", spec);
}

static const char *generic_name(D generic) {
    D symbol = ((DyGeneric *)DY_PTR(generic))->name;
    if (symbol == DY_FALSE) return "an anonymous generic function";
    return dy_symbol_utf8(symbol);
}

_Noreturn void dy_no_applicable_method(D generic) {
    dy_error_naming("no applicable method for ", generic_name(generic), "", 0);
}

static bool dropped(D x, D method, const D *replaced, intptr_t n) {
    if (x == method) return true;
    for (intptr_t j = 0; j < n; j++)
        if (x == replaced[j]) return true;
    return false;
}

/* Held while a method is attached (dylan.h) */
static pthread_mutex_t methods_lock = PTHREAD_MUTEX_INITIALIZER;

void dy_lock_methods(void) { pthread_mutex_lock(&methods_lock); }
void dy_unlock_methods(void) { pthread_mutex_unlock(&methods_lock); }

void dy_check_addable(D generic) {
    if (((DyGeneric *)DY_PTR(generic))->read_only)
        dy_error("the generic function is read-only", generic);
}

void dy_add_method(D generic, D method, const D *replaced, intptr_t n) {
    DyGeneric *g = DY_PTR(generic);
    DyVector *old = DY_PTR(g->methods);
    intptr_t size = DY_INT_VALUE(old->size), kept = 0;
    for (intptr_t i = 0; i < size; i++)
        if (!dropped(old->data[i], method, replaced, n)) kept++;
    D v = dy_make_vector((size_t)kept + 1, DY_FALSE);
    DyVector *new = DY_PTR(v);
    intptr_t k = 0;
    for (intptr_t i = 0; i < size; i++)
        if (!dropped(old->data[i], method, replaced, n)) new->data[k++] = old->data[i];
    new->data[k] = method;
    g->methods = v;
}

void dy_remove_methods(D generic, const D *removed, intptr_t n) {
    DyGeneric *g = DY_PTR(generic);
    DyVector *old = DY_PTR(g->methods);
    intptr_t size = DY_INT_VALUE(old->size), kept = 0;
    for (intptr_t i = 0; i < size; i++)
        if (!dropped(old->data[i], DY_UNBOUND, removed, n)) kept++;
    D v = dy_make_vector((size_t)kept, DY_FALSE);
    DyVector *new = DY_PTR(v);
    intptr_t k = 0;
    for (intptr_t i = 0; i < size; i++)
        if (!dropped(old->data[i], DY_UNBOUND, removed, n)) new->data[k++] = old->data[i];
    g->methods = v;
}

/* The variable NAME, which define-method defines, holds VALUE; it must be
   GENERIC, the generic function the compiler made for it (chapter 6). */
void dy_check_generic_variable(D value, D generic, const char *name) {
    if (value == generic) return;
    if (!dy_kind_p(value, DY_KIND_GENERIC))
        dy_error_naming("", name, dy_kind_p(value, DY_KIND_CLASS)
                        ? " is defined both as a class and otherwise"
                        : " is defined both as a variable and as a generic function", 0);
    dy_error_naming("", name, " holds another generic function, which a compiled program cannot add a method to", 0);
}

/* Do the keys of two parameter lists, as function-arguments has them --
   #f, () for #key alone, or a list of keywords -- agree? */
static bool same_keys(D a, D b) {
    if (a == DY_FALSE || b == DY_FALSE) return a == b;
    if (a == DY_EMPTY || b == DY_EMPTY) return a == b;
    for (D l = a; l != DY_EMPTY; l = dy_tail(l)) {
        D m = b;
        while (m != DY_EMPTY && dy_head(m) != dy_head(l)) m = dy_tail(m);
        if (m == DY_EMPTY) return false;
    }
    for (D l = b; l != DY_EMPTY; l = dy_tail(l)) {
        D m = a;
        while (m != DY_EMPTY && dy_head(m) != dy_head(l)) m = dy_tail(m);
        if (m == DY_EMPTY) return false;
    }
    return true;
}

/* A define-generic-function of a variable a module imports from another,
   which holds its value already (chapter 6): it must be a generic
   function, and the declaration consistent with it. */
void dy_redeclare_generic(D value, const char *name, intptr_t required, bool rest, D keys) {
    if (!dy_kind_p(value, DY_KIND_GENERIC))
        dy_error_naming("", name, dy_kind_p(value, DY_KIND_CLASS)
                        ? " is defined both as a class and otherwise"
                        : " is defined both as a variable and as a generic function", 0);
    DyVector *info = DY_PTR(((DyGeneric *)DY_PTR(value))->info);
    if (DY_INT_VALUE(info->data[0]) != required || (info->data[1] != DY_FALSE) != rest
        || !same_keys(info->data[2], keys))
        dy_error_naming("define-generic-function: ", name,
                        " is inconsistent with the existing generic function", 0);
}

_Noreturn void dy_unbound_slot(const char *getter) {
    dy_error_naming("the slot ", getter, " is not initialized", 0);
}

_Noreturn void dy_not_a_slot(const char *getter, D instance) {
    dy_error_naming("", getter, " is not a slot of the object", instance);
}

void dy_check_keywords(D f, intptr_t argc, intptr_t start, D a0, D a1, D a2,
                       D a3, const D *accepted, size_t n) {
    D in_registers[4] = { a0, a1, a2, a3 };
    if ((argc - start) % 2 != 0) dy_odd_keyword_arguments(f);
    for (intptr_t i = start; i < argc; i += 2) {
        D key = i < 4 ? in_registers[i] : *dy_argument_slot(i);
        if (!dy_kind_p(key, DY_KIND_KEYWORD)) dy_odd_keyword_arguments(f);
        if (accepted) {
            size_t j = 0;
            while (j < n && accepted[j] != key) j++;
            if (j == n) dy_unknown_keyword(f, key);
        }
    }
}

void dy_check_keyword_list(D f, D pairs, const D *accepted, size_t n) {
    for (D l = pairs; l != DY_EMPTY; l = dy_tail(dy_tail(l))) {
        D key = dy_head(l);
        if (!dy_kind_p(key, DY_KIND_KEYWORD)) dy_odd_keyword_arguments(f);
        size_t j = 0;
        while (j < n && accepted[j] != key) j++;
        if (j == n) dy_unknown_keyword(f, key);
    }
}

/* Is the type A, a class or singleton, within the type B? */
static bool type_within(D a, D b) {
    if (dy_kind_p(a, DY_KIND_SINGLETON))
        return dy_specializer_matches(((DySingleton *)DY_PTR(a))->object, b);
    return dy_kind_p(b, DY_KIND_CLASS)
        && dy_wrapper_subtype(((DyClass *)DY_PTR(a))->instance_wrapper,
                              ((DyClass *)DY_PTR(b))->instance_wrapper);
}

/* Add to the format string at OUT the text TEXT, each ~ doubled. */
static char *add_text(char *out, const char *text) {
    for (; *text; text++) {
        *out++ = *text;
        if (*text == '~') *out++ = '~';
    }
    return out;
}

/* Add to the format string at OUT the type T: a class by its name, a
 * singleton as (singleton ~s), its object added to the end of *ARGUMENTS. */
static char *add_type(char *out, D t, D *arguments) {
    if (dy_kind_p(t, DY_KIND_SINGLETON)) {
        D cell = dy_pair(((DySingleton *)DY_PTR(t))->object, DY_EMPTY);
        if (*arguments == DY_EMPTY) *arguments = cell;
        else dy_set_tail(*arguments, cell);
        strcpy(out, "(singleton ~s)");
        return out + strlen(out);
    }
    D name = ((DyClass *)DY_PTR(t))->name;
    return add_text(out, name == DY_FALSE ? "an anonymous class" : dy_symbol_utf8(name));
}

static size_t type_text_length(D t) {
    if (dy_kind_p(t, DY_KIND_SINGLETON)) return 14;
    D name = ((DyClass *)DY_PTR(t))->name;
    return name == DY_FALSE ? 18 : strlen(dy_symbol_utf8(name));
}

void dy_check_slot_type_within(D type, D inherited, const char *getter) {
    if (inherited == DY_UNBOUND || type_within(type, inherited)) return;
    size_t room = 2 * (strlen(getter) + type_text_length(type) + type_text_length(inherited))
        + 80;
    char *format = dy_alloc_bytes(room), *out = format;
    D arguments = DY_EMPTY;
    out = add_text(out, "slot ");
    out = add_text(out, getter);
    out = add_text(out, ": the new slot type ");
    out = add_type(out, type, &arguments);
    out = add_text(out, " is not a subtype of the inherited type ");
    out = add_type(out, inherited, &arguments);
    *out = 0;
    dy_error_format(format, arguments);
}

/* The name of the slot D, its getter's */
static const char *slot_name(DySlotDescriptor *s) {
    D getter = s->getter;
    if (dy_kind_p(getter, DY_KIND_GENERIC))
        return dy_symbol_utf8(((DyGeneric *)DY_PTR(getter))->name);
    return "?";
}

static DySlotDescriptor *descriptor(D d);

/* The check of dy_check_slot_respecified, against the N descriptors OLDS
 * (#f for none) */
static void check_respecified(D d, const D *olds, intptr_t n) {
    DySlotDescriptor *s = descriptor(d);
    if (s->kind == 4) return;
    const char *name = slot_name(s);
    for (intptr_t i = 0; i < n; i++) {
        D old = olds[i];
        if (old == DY_FALSE) continue;
        DySlotDescriptor *o = descriptor(old);
        const char *missing = NULL;
        if (o->type != DY_UNBOUND && s->type == DY_UNBOUND) missing = "type:";
        else if ((o->init_value != DY_UNBOUND || o->init_function != DY_UNBOUND)
                 && s->init_value == DY_UNBOUND && s->init_function == DY_UNBOUND)
            missing = "init-value: or init-function:";
        else if (o->init_keyword != DY_FALSE && s->init_keyword == DY_FALSE)
            missing = "init-keyword:";
        else if (o->kind != 0 && s->kind == 0) missing = "allocation:";
        else if (o->setter != DY_FALSE && s->setter == DY_FALSE) missing = "setter:";
        if (missing) {
            char *format = dy_alloc_bytes(2 * strlen(name) + 80), *out = format;
            out = add_text(out, "slot ");
            out = add_text(out, name);
            out = add_text(out, ": redefining an inherited slot must respecify its ");
            out = add_text(out, missing);
            *out = 0;
            dy_error_format(format, DY_EMPTY);
        }
    }
    if (s->type == DY_UNBOUND) return;
    for (intptr_t i = 0; i < n; i++)
        if (olds[i] != DY_FALSE && descriptor(olds[i])->type != DY_UNBOUND)
            dy_check_slot_type_within(s->type, descriptor(olds[i])->type, name);
}

void dy_check_slot_respecified(D d, const D *supers, intptr_t n) {
    D olds[n > 0 ? n : 1];
    for (intptr_t i = 0; i < n; i++)
        olds[i] = dy_find_slot_descriptor(supers[i], descriptor(d)->getter);
    check_respecified(d, olds, n);
}

void dy_check_class_slots(const D *supers, intptr_t n, const D *own, intptr_t m) {
    for (intptr_t i = 0; i < m; i++) dy_check_slot_respecified(own[i], supers, n);
}

/* ------------------------------------------------------- Next methods */

/* The changes to which methods are attached (dylan.h): DY_METHODS_NOW is
 * a #[address, old value, next] whose next is #f, which the next change
 * fills.  A method entered holds the one current then; those no method
 * holds are garbage. */
D dy_methods_now = DY_FALSE;

static DyNextIndex *next_index;
static intptr_t next_count;

static int by_next(const void *a, const void *b) {
    uintptr_t x = (uintptr_t)((const DyNextIndex *)a)->next;
    uintptr_t y = (uintptr_t)((const DyNextIndex *)b)->next;
    return x < y ? -1 : x > y;
}

void dy_set_next_index(DyNextIndex *index) {
    intptr_t n = 0;
    while (index[n].next) n++;
    qsort(index, (size_t)n, sizeof *index, by_next);
    next_index = index;
    next_count = n;
    if (dy_methods_now == DY_FALSE) dy_methods_now = dy_make_vector(3, DY_FALSE);
}

static const DyNextIndex *next_steps(D next) {
    intptr_t low = 0, high = next_count;
    while (low < high) {
        intptr_t mid = low + (high - low) / 2;
        uintptr_t m = (uintptr_t)next_index[mid].next;
        if (m == (uintptr_t)next) return &next_index[mid];
        if (m < (uintptr_t)next) low = mid + 1; else high = mid;
    }
    return NULL;
}

static void note_change(void *address, D old) {
    if (dy_methods_now == DY_FALSE) dy_methods_now = dy_make_vector(3, DY_FALSE);
    D fresh = dy_make_vector(3, DY_FALSE);
    DyVector *v = DY_PTR(dy_methods_now);
    v->data[0] = DY_INT((intptr_t)address);
    v->data[1] = old;
    __atomic_store_n(&v->data[2], fresh, __ATOMIC_RELEASE);
    __atomic_store_n(&dy_methods_now, fresh, __ATOMIC_RELEASE);
}

void dy_set_method_flag(unsigned char *flag, int on) {
    if (*flag == on) return;
    note_change(flag, DY_INT(*flag));
    __atomic_store_n(flag, (unsigned char)on, __ATOMIC_RELEASE);
}

void dy_set_method_closure(D *variable, D closure) {
    if (*variable == closure) return;
    note_change(variable, *variable);
    __atomic_store_n(variable, closure, __ATOMIC_RELEASE);
}

/* What was at ADDRESS, which holds NOW, when AT was current: the old value
 * of its first change after.  NOW is read first: a change is noted before
 * it is made. */
static D value_at(const void *address, D now, D at) {
    D key = DY_INT((intptr_t)address);
    for (D n = at;;) {
        DyVector *v = DY_PTR(n);
        D next = __atomic_load_n(&v->data[2], __ATOMIC_ACQUIRE);
        if (next == DY_FALSE) return now;
        if (v->data[0] == key) return v->data[1];
        n = next;
    }
}

static int attached_at(const DyNextStep *s, D at) {
    if (!s->flag) return 1;
    D now = DY_INT(__atomic_load_n(s->flag, __ATOMIC_ACQUIRE));
    return value_at(s->flag, now, at) != DY_INT(0);
}

/* env[0] is a compiled next method, env[1] the place it runs as of */
static D next_as_of_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    DyFunction *c = DY_PTR(self);
    D at = c->env[1];
    const DyNextIndex *x = next_steps(c->env[0]);
    for (intptr_t i = 0; i < x->count; i++) {
        const DyNextStep *s = &x->steps[i];
        if (!attached_at(s, at)) continue;
        D object = s->object;
        if (s->closure)
            object = value_at(s->closure, __atomic_load_n(s->closure, __ATOMIC_ACQUIRE), at);
        D next = DY_FALSE;
        if (s->next) {
            const DyNextIndex *r = next_steps(DY_OBJ(s->next));
            for (intptr_t j = 0; j < r->count; j++)
                if (attached_at(&r->steps[j], at)) {
                    next = dy_next_as_of(DY_OBJ(s->next), at);
                    break;
                }
        }
        dy_thread->next_method = next;
        if (s->entry) DY_TAIL_CALL_DIRECT(s->entry, object, a0, a1, a2, a3, argc);
        DY_TAIL_CALL_DIRECT(dy_entry(object), object, a0, a1, a2, a3, argc);
    }
    /* (Not reached: a next method is given only if one was attached.) */
    dy_no_applicable_method(x->generic);
}

D dy_next_as_of(D next, D at) {
    if (next == DY_FALSE || at == __atomic_load_n(&dy_methods_now, __ATOMIC_ACQUIRE)
        || !next_steps(next))
        return next;
    D c = dy_make_closure(next_as_of_entry, 2);
    DyFunction *f = DY_PTR(c);
    f->env[0] = next;
    f->env[1] = at;
    return c;
}

/* env[0] is the next method, env[1] the place it runs as of */
static D kept_next_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    DyFunction *c = DY_PTR(self);
    D next = dy_next_as_of(c->env[0], c->env[1]);
    DY_TAIL_CALL(next, a0, a1, a2, a3, argc);
}

D dy_keep_next(D next, D at) {
    if (next == DY_FALSE || !next_steps(next)) return next;
    D c = dy_make_closure(kept_next_entry, 2);
    DyFunction *f = DY_PTR(c);
    f->env[0] = next;
    f->env[1] = at;
    return c;
}

/* env[0] is the next method, env[1] the place it runs as of, env[2] the
 * argument count, env[3..] the arguments. */
static D captured_next_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    DyFunction *c = DY_PTR(self);
    D next = dy_next_as_of(c->env[0], c->env[1]);
    if (argc == 0) {
        argc = DY_INT_VALUE(c->env[2]);
        dy_reserve_arguments(argc);
        for (intptr_t i = 4; i < argc; i++) *dy_argument_slot(i) = c->env[3 + i];
        a0 = argc > 0 ? c->env[3] : 0;
        a1 = argc > 1 ? c->env[4] : 0;
        a2 = argc > 2 ? c->env[5] : 0;
        a3 = argc > 3 ? c->env[6] : 0;
    }
    DY_TAIL_CALL(next, a0, a1, a2, a3, argc);
}

D dy_capture_next(D next, D at, intptr_t argc, D a0, D a1, D a2, D a3) {
    size_t n = argc < 4 ? 4 : (size_t)argc;
    D c = dy_make_closure(captured_next_entry, 3 + n);
    DyFunction *f = DY_PTR(c);
    f->env[0] = next;
    f->env[1] = at;
    f->env[2] = DY_INT(argc);
    f->env[3] = a0; f->env[4] = a1; f->env[5] = a2; f->env[6] = a3;
    for (intptr_t i = 4; i < argc; i++) f->env[3 + i] = *dy_argument_slot(i);
    return c;
}

/* ---------------------------------------------------------- Instances */

D dy_make_instance(DyWrapper *wrapper) {
    DyInstance *x = dy_alloc(wrapper, DY_INSTANCE_BYTES(wrapper->size));
    for (uint32_t i = 0; i < wrapper->size; i++) x->slots[i] = DY_UNBOUND;
    return DY_OBJ(x);
}

D dy_find_keyword(D pairs, D keyword) {
    for (D l = pairs; l != DY_EMPTY; l = dy_tail(dy_tail(l)))
        if (dy_head(l) == keyword) return dy_head(dy_tail(l));
    return DY_UNSUPPLIED;
}

D dy_call_list(D f, D first, D list) {
    DY_CHECK_STACK();
    intptr_t n = 1;
    for (D l = list; l != DY_EMPTY; l = dy_tail(l)) n++;
    dy_reserve_arguments(n);
    D a[4] = { first, 0, 0, 0 };
    intptr_t i = 1;
    for (D l = list; l != DY_EMPTY; l = dy_tail(l), i++) {
        if (i < 4) a[i] = dy_head(l);
        else *dy_argument_slot(i) = dy_head(l);
    }
    dy_thread->next_method = DY_FALSE;
    return DY_CALL(f, a[0], a[1], a[2], a[3], n);
}

/* initialize's method on <object> (chapter 6: instance #key...): the
 * init arguments must be keyword/value pairs, of any keywords. */
static D default_initialize_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    dy_check_keywords(self, argc, 1, a0, a1, a2, a3, NULL, 0);
    return a0;
}

DyFunction dy_default_initialize = { &dy_wrapper_method, default_initialize_entry,
                                     DY_FALSE, DY_FALSE, DY_FALSE, NULL };

/* ------------------------------------------- Slots define-slot may change */

static DySlotDescriptor *descriptor(D d) { return (DySlotDescriptor *)DY_PTR(d); }

/* Is the class A the class B or a subclass of it? */
static bool class_within(D a, D b) {
    return dy_wrapper_subtype(((DyClass *)DY_PTR(a))->instance_wrapper,
                              ((DyClass *)DY_PTR(b))->instance_wrapper);
}

static intptr_t slot_position(const D *slots, intptr_t n, D getter) {
    for (intptr_t i = 0; i < n; i++)
        if (descriptor(slots[i])->getter == getter) return i;
    return -1;
}

static D copy_slots(D work, intptr_t n) {
    D v = dy_make_vector((size_t)n, DY_FALSE);
    memcpy(DY_VECTOR_DATA(v), DY_VECTOR_DATA(work), (size_t)n * sizeof(D));
    return v;
}

D dy_combine_slots(const D *supers, intptr_t n, const D *own, intptr_t m) {
    intptr_t total = m;
    for (intptr_t i = 0; i < n; i++)
        if (supers[i] != DY_UNBOUND) total += DY_VECTOR_SIZE(supers[i]);
    D work = dy_make_vector((size_t)total, DY_FALSE);
    D *r = DY_VECTOR_DATA(work);
    intptr_t count = 0;
    for (intptr_t i = 0; i < n; i++) {
        if (supers[i] == DY_UNBOUND) continue;
        intptr_t size = DY_VECTOR_SIZE(supers[i]);
        const D *data = DY_VECTOR_DATA(supers[i]);
        for (intptr_t j = 0; j < size; j++) {
            D d = data[j], getter = descriptor(d)->getter;
            intptr_t at = slot_position(r, count, getter);
            if (at < 0) { r[count++] = d; continue; }
            if (slot_position(own, m, getter) >= 0) continue;   /* the class's own */
            D origin = descriptor(d)->owner, was = descriptor(r[at])->owner;
            if (origin != was && !class_within(was, origin) && class_within(origin, was))
                r[at] = d;
        }
    }
    for (intptr_t i = 0; i < m; i++) {
        intptr_t at = slot_position(r, count, descriptor(own[i])->getter);
        if (at >= 0) r[at] = own[i];
        else r[count++] = own[i];
    }
    return copy_slots(work, count);
}

D dy_reported_slots(D slots) {
    if (slots == DY_UNBOUND) return dy_make_vector(0, DY_FALSE);
    intptr_t n = DY_VECTOR_SIZE(slots), kept = 0;
    for (intptr_t i = 0; i < n; i++)
        if (!descriptor(DY_VECTOR_DATA(slots)[i])->hidden) kept++;
    D v = dy_make_vector((size_t)kept, DY_FALSE);
    D *out = DY_VECTOR_DATA(v), *in = DY_VECTOR_DATA(slots);
    for (intptr_t i = 0, k = 0; i < n; i++)
        if (!descriptor(in[i])->hidden) out[k++] = in[i];
    return v;
}

D dy_find_slot_descriptor(D slots, D getter) {
    if (slots == DY_UNBOUND) return DY_FALSE;
    intptr_t at = slot_position(DY_VECTOR_DATA(slots), DY_VECTOR_SIZE(slots), getter);
    return at < 0 ? DY_FALSE : DY_VECTOR_DATA(slots)[at];
}

/* SLOTS with D in the place of the one with its getter, or after the rest */
static D put_slot(D slots, D d) {
    if (slots == DY_UNBOUND) slots = dy_make_vector(0, DY_FALSE);
    intptr_t n = DY_VECTOR_SIZE(slots);
    intptr_t at = slot_position(DY_VECTOR_DATA(slots), n, descriptor(d)->getter);
    D v = dy_make_vector((size_t)(at < 0 ? n + 1 : n), DY_FALSE);
    memcpy(DY_VECTOR_DATA(v), DY_VECTOR_DATA(slots), (size_t)n * sizeof(D));
    DY_VECTOR_DATA(v)[at < 0 ? n : at] = d;
    return v;
}

/* The early instances (weak lists), and the counts of the define-slots
 * that may still run, with what changes them in other threads */
static pthread_mutex_t early_lock = PTHREAD_MUTEX_INITIALIZER;

/* Give each instance of K made early, and alive, the initial value of the
 * slot D */
static void initialize_early(const DyProgramClass *k, D d) {
    DY_CHECK_STACK();
    pthread_mutex_lock(&early_lock);
    D early = dy_weak_members(*k->early);
    pthread_mutex_unlock(&early_lock);
    for (; early != DY_EMPTY; early = dy_tail(early)) {
        DySlotDescriptor *s = descriptor(d);
        D value = s->init_function != DY_UNBOUND
            ? DY_CALL(s->init_function, 0, 0, 0, 0, 0) : s->init_value;
        if (value != DY_UNBOUND && s->type != DY_UNBOUND) dy_check_type(value, s->type);
        D *place = dy_instance_slot(dy_head(early), (int32_t)s->number);
        if (place) *place = value;
    }
}

/* Append to QUEUE (of *SIZE, with room for *ROOM) the subclasses of K
 * defined so far, the most recently defined first */
static const DyProgramClass **queue_subclasses(const DyProgramClass **queue, intptr_t *size,
                                               intptr_t *room, const DyProgramClass *k) {
    intptr_t start = *size;
    for (intptr_t i = 0; i < k->subclass_count; i++) {
        const DyProgramClass *c = k->subclasses[i];
        if (!*c->ready) continue;
        if (*size == *room) {
            *room = *room * 2 + 8;
            queue = realloc(queue, (size_t)*room * sizeof *queue);
            if (!queue) dy_error("out of memory", 0);
        }
        intptr_t j = (*size)++;
        while (j > start && *queue[j - 1]->ready < *c->ready) { queue[j] = queue[j - 1]; j--; }
        queue[j] = c;
    }
    return queue;
}

void dy_check_slot_owner(D class) {
    DyClass *c = DY_PTR(class);
    /* A sealed class is read-only too (chapter 11) */
    if (c->read_only || c->sealed) dy_error("define-slot: the class is read-only", class);
}

void dy_check_define_slot(const DyProgramClass *owner, D d, const D *supers, intptr_t n) {
    D current = dy_find_slot_descriptor(*owner->slots, descriptor(d)->getter);
    if (current != DY_FALSE && descriptor(current)->owner != owner->class)
        dy_check_slot_respecified(d, supers, n);
}

void dy_define_class_slot(const DyProgramClass *owner, D d) {
    dy_check_slot_owner(owner->class);
    D getter = descriptor(d)->getter;
    bool instance = descriptor(d)->kind == 0;
    /* The slot replaced, if any, which the subclasses that inherit it
     * share */
    D replaced = dy_find_slot_descriptor(*owner->slots, getter);
    *owner->slots = put_slot(*owner->slots, d);
    if (instance) initialize_early(owner, d);
    /* Every instance of the owner is given the slot (chapter 6): down
     * through the subclasses, breadth first, as far as each lacks a slot
     * with the getter or inherits the one replaced */
    intptr_t size = 0, room = 0, next = 0, seen_size = 0;
    const DyProgramClass **queue = queue_subclasses(NULL, &size, &room, owner);
    const DyProgramClass **seen = malloc((size_t)(room + 1) * sizeof *seen);
    intptr_t seen_room = room + 1;
    if (!seen) dy_error("out of memory", 0);
    while (next < size) {
        const DyProgramClass *c = queue[next++];
        bool done = false;
        for (intptr_t i = 0; i < seen_size; i++) if (seen[i] == c) done = true;
        if (done) continue;
        if (seen_size == seen_room) {
            seen_room = seen_room * 2;
            seen = realloc(seen, (size_t)seen_room * sizeof *seen);
            if (!seen) dy_error("out of memory", 0);
        }
        seen[seen_size++] = c;
        D slots = *c->slots;
        D had = dy_find_slot_descriptor(slots, getter);
        if (had != DY_FALSE && had != replaced) {
            /* The subclass's own redefinition must still respecify what
             * the new definition specifies */
            if (descriptor(had)->owner == c->class) check_respecified(had, &d, 1);
            continue;
        }
        *c->slots = put_slot(slots, d);
        if (instance) initialize_early(c, d);
        queue = queue_subclasses(queue, &size, &room, c);
    }
    free(queue);
    free(seen);
}

void dy_define_slot_done(const DyProgramClass *k) {
    pthread_mutex_lock(&early_lock);
    if (--*k->pending == 0) {
        dy_weak_drop(*k->early);
        *k->early = DY_EMPTY;
    }
    pthread_mutex_unlock(&early_lock);
}

/* -------------------------------------------- Slots of native objects */

/* The instance slots define-slot gives a built-in class (chapter 6) that
 * are those of objects that are not instances -- symbols, characters,
 * numbers, functions and the rest, which have no storage of their own.
 * Each slot's getter has a table of them, placed by the object's identity
 * (id?): a <simple-object-vector> of the count of places used, weak keys
 * (dy_weak_keys), and each place's strong key and value.  A place's value
 * is DY_UNBOUND while it is unused.  An object in the heap is held by the
 * weak keys only, its strong key DY_UNBOUND, so that its slot does not
 * keep it alive: when it dies its place is dead, and is dropped when the
 * table is next rebuilt.  Any other object -- an integer, a character, an
 * object outside the heap, or a number, which is id? to every number of
 * its value -- is its place's strong key.  An object's slot takes its
 * initial value when first read. */

static pthread_mutex_t native_lock = PTHREAD_MUTEX_INITIALIZER;

/* A hash of X: the same for objects id? to each other */
static size_t identity_hash(D x) {
    uint64_t h;
    if (DY_IS_INT(x)) h = (uint64_t)DY_INT_VALUE(x);
    else if (DY_IS_POINTER(x) && dy_number_p(x)) {
        /* Numbers of a value are one object (dy_identical) */
        double v = dy_float_value(x);
        memcpy(&h, &v, sizeof h);
    } else {
#ifdef DY_MPS
        h = (uint64_t)DY_INT_VALUE(dy_object_hash(x));
#else
        h = (uint64_t)x;
#endif
    }
    return (size_t)((h * 0x9E3779B97F4A7C15u) >> 16);
}

static intptr_t native_capacity(D table) { return (DY_VECTOR_SIZE(table) - 2) / 2; }

/* Place I of TABLE: its strong key, then its value */
static D *native_place(D table, intptr_t i) { return &DY_VECTOR_DATA(table)[2 + 2 * i]; }

/* Is X held by the weak keys? */
static bool weakly_held(D x) { return dy_in_heap(x) && !dy_number_p(x); }

/* The key of the used place P, I of TABLE: 0 if its object has died */
static D native_key(D table, intptr_t i, D *p) {
    return p[0] != DY_UNBOUND ? p[0] : dy_weak_key(DY_VECTOR_DATA(table)[1], i);
}

/* The index in TABLE of X's place, or of the unused one where it would go */
static intptr_t native_find(D table, D x) {
    intptr_t capacity = native_capacity(table);
    for (intptr_t i = (intptr_t)(identity_hash(x) & (size_t)(capacity - 1));;
         i = (i + 1) & (capacity - 1)) {
        D *p = native_place(table, i);
        if (p[1] == DY_UNBOUND) return i;
        D key = native_key(table, i, p);
        if (key && dy_identical(key, x)) return i;
    }
}

static D make_native_table(intptr_t capacity) {
    D keys = dy_weak_keys(capacity);
    D table = dy_make_vector((size_t)(2 + 2 * capacity), DY_UNBOUND);
    DY_VECTOR_DATA(table)[0] = DY_INT(0);
    DY_VECTOR_DATA(table)[1] = keys;
    return table;
}

/* X in place I of TABLE, unused, with VALUE */
static void native_fill(D table, intptr_t i, D x, D value) {
    D *p = native_place(table, i);
    if (weakly_held(x)) dy_set_weak_key(DY_VECTOR_DATA(table)[1], i, x);
    else p[0] = x;
    p[1] = value;
    D *count = &DY_VECTOR_DATA(table)[0];
    *count = DY_INT(DY_INT_VALUE(*count) + 1);
}

/* OLD's places whose objects are alive, in a new table, twice the size
 * if they fill more than a quarter of it */
static D rebuild_native_table(D old) {
    intptr_t capacity = native_capacity(old);
    intptr_t alive = 0;
    for (intptr_t i = 0; i < capacity; i++) {
        D *p = native_place(old, i);
        if (p[1] != DY_UNBOUND && native_key(old, i, p)) alive++;
    }
    D new = make_native_table((alive + 1) * 4 > capacity ? capacity * 2 : capacity);
    for (intptr_t i = 0; i < capacity; i++) {
        D *p = native_place(old, i);
        if (p[1] == DY_UNBOUND) continue;
        D key = native_key(old, i, p);  /* on the stack: alive while it moves */
        if (key) native_fill(new, native_find(new, key), key, p[1]);
    }
    dy_weak_keys_drop(DY_VECTOR_DATA(old)[1]);
    return new;
}

/* *TABLE with X's value VALUE.  NATIVE_LOCK is held. */
static void native_put(D *table, D x, D value) {
    if (*table == DY_UNBOUND) *table = make_native_table(8);
    intptr_t i = native_find(*table, x);
    D *p = native_place(*table, i);
    if (p[1] != DY_UNBOUND) {
        p[1] = value;
        return;
    }
    /* A new place: the dead dropped first if the table is half used */
    if ((DY_INT_VALUE(DY_VECTOR_DATA(*table)[0]) + 1) * 2 > native_capacity(*table)) {
        *table = rebuild_native_table(*table);
        i = native_find(*table, x);
    }
    native_fill(*table, i, x, value);
}

/* X's place in *TABLE: true, with its value in *VALUE, if X has one */
static bool native_get(D *table, D x, D *value) {
    pthread_mutex_lock(&native_lock);
    bool found = false;
    if (*table != DY_UNBOUND) {
        D *p = native_place(*table, native_find(*table, x));
        if (p[1] != DY_UNBOUND) {
            *value = p[1];
            found = true;
        }
    }
    pthread_mutex_unlock(&native_lock);
    return found;
}

D dy_native_slot(D *table, D x, D d) {
    DY_CHECK_STACK();
    D value;
    if (native_get(table, x, &value)) return value;
    DySlotDescriptor *s = descriptor(d);
    value = s->init_function != DY_UNBOUND ? DY_CALL(s->init_function, 0, 0, 0, 0, 0)
                                           : s->init_value;
    if (value != DY_UNBOUND && s->type != DY_UNBOUND) dy_check_type(value, s->type);
    /* The init-function may have given X the slot already */
    D had;
    if (native_get(table, x, &had)) return had;
    if (value == DY_UNBOUND) return value;
    pthread_mutex_lock(&native_lock);
    native_put(table, x, value);
    pthread_mutex_unlock(&native_lock);
    return value;
}

void dy_set_native_slot(D *table, D x, D value) {
    pthread_mutex_lock(&native_lock);
    native_put(table, x, value);
    pthread_mutex_unlock(&native_lock);
}

bool dy_native_slot_initialized(D *table, D x, D d) {
    D value;
    if (native_get(table, x, &value)) return value != DY_UNBOUND;
    DySlotDescriptor *s = descriptor(d);
    return s->init_function != DY_UNBOUND || s->init_value != DY_UNBOUND;
}

/* The slot whose storage D, one of K's slots, initializes: D, or for a
 * virtual slot the one it keeps; NULL if none */
static DySlotDescriptor *storage_of(const DyProgramClass *k, D d) {
    DySlotDescriptor *s = descriptor(d);
    if (s->kind != 4) return s;
    for (intptr_t i = 0; i < k->storage_count; i++)
        if (descriptor(k->storage[i])->getter == s->getter) return descriptor(k->storage[i]);
    return NULL;
}

/* As the interpreter makes an instance: the required init-keywords first,
 * then each slot in order, then the keywords nothing takes */
D dy_fill_slots(D x, const DyProgramClass *k, D self, intptr_t argc, D a0, D a1,
                D a2, D a3) {
    DY_CHECK_STACK();
    D slots = *k->slots;
    intptr_t n = DY_VECTOR_SIZE(slots);
    dy_check_keywords(self, argc, 1, a0, a1, a2, a3, NULL, 0);
    D keys = dy_collect_arguments(argc, 1, a0, a1, a2, a3);
    for (intptr_t i = 0; i < n; i++) {
        DySlotDescriptor *s = storage_of(k, DY_VECTOR_DATA(slots)[i]);
        if (s && s->kind == 0 && s->required
            && dy_find_keyword(keys, s->init_keyword) == DY_UNSUPPLIED)
            dy_error("make: the required init-keyword is missing", s->init_keyword);
    }
    for (intptr_t i = 0; i < n; i++) {
        DySlotDescriptor *s = storage_of(k, DY_VECTOR_DATA(slots)[i]);
        if (!s || s->kind != 0) continue;
        D v = s->init_keyword != DY_FALSE ? dy_find_keyword(keys, s->init_keyword) : DY_UNSUPPLIED;
        if (v == DY_UNSUPPLIED)
            v = s->init_function != DY_UNBOUND ? DY_CALL(s->init_function, 0, 0, 0, 0, 0)
                                               : s->init_value;
        if (v == DY_UNBOUND) continue;
        if (s->type != DY_UNBOUND) dy_check_type(v, s->type);
        D *place = dy_instance_slot(x, (int32_t)s->number);
        if (place) *place = v;
    }
    bool any = k->any_keys;
    for (intptr_t i = 0; !any && i < k->any_flag_count; i++) any = *k->any_flags[i];
    if (!any) {
        D accepted = dy_make_vector((size_t)(n + k->init_key_count), DY_FALSE);
        D *names = DY_VECTOR_DATA(accepted);
        intptr_t count = 0;
        for (intptr_t i = 0; i < n; i++) {
            DySlotDescriptor *s = storage_of(k, DY_VECTOR_DATA(slots)[i]);
            if (s && s->init_keyword != DY_FALSE) names[count++] = s->init_keyword;
        }
        for (intptr_t i = 0; i < k->init_key_count; i++)
            if (!k->init_key_flags || !k->init_key_flags[i] || *k->init_key_flags[i])
                names[count++] = k->init_keys[i];
        /* From KEYS: the init functions may have reused the argument area */
        dy_check_keyword_list(self, keys, names, (size_t)count);
    }
    if (__atomic_load_n(k->pending, __ATOMIC_RELAXED) > 0) {
        pthread_mutex_lock(&early_lock);
        if (*k->pending > 0) {
            *k->early = dy_weak_add(*k->early, x);
            /* For a copy of X (dy_note_copy) */
            DyClass *c = DY_PTR(dy_object_class(x));
            if (c == DY_PTR(k->class)) c->program = k;
        }
        pthread_mutex_unlock(&early_lock);
    }
    return keys;
}

void dy_note_copy(D copy) {
    const DyProgramClass *k = ((DyClass *)DY_PTR(dy_object_class(copy)))->program;
    if (!k || __atomic_load_n(k->pending, __ATOMIC_RELAXED) == 0) return;
    pthread_mutex_lock(&early_lock);
    if (*k->pending > 0) *k->early = dy_weak_add(*k->early, copy);
    pthread_mutex_unlock(&early_lock);
}
