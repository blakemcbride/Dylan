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
        dy_error_naming("", name, " is defined both as a variable and as a generic function", 0);
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
        dy_error_naming("", name, " is defined both as a variable and as a generic function", 0);
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

/* ------------------------------------------------------- Next methods */

/* env[0] is the next method, env[1] the argument count, env[2..] the
 * arguments. */
static D captured_next_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    DyFunction *c = DY_PTR(self);
    D next = c->env[0];
    if (argc == 0) {
        argc = DY_INT_VALUE(c->env[1]);
        dy_reserve_arguments(argc);
        for (intptr_t i = 4; i < argc; i++) *dy_argument_slot(i) = c->env[2 + i];
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
    for (intptr_t i = 4; i < argc; i++) f->env[2 + i] = *dy_argument_slot(i);
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

void dy_define_class_slot(const DyProgramClass *owner, D d) {
    if (((DyClass *)DY_PTR(owner->class))->read_only)
        dy_error("define-slot: the class is read-only", owner->class);
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
        if (had != DY_FALSE && had != replaced) continue;
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
        if (*k->pending > 0) *k->early = dy_weak_add(*k->early, x);
        pthread_mutex_unlock(&early_lock);
    }
    return keys;
}
