/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * The Dylan runtime: how values are represented (CompilerPlan.md, Phase 3).
 *
 * Every Dylan value is one machine word, a D.
 *
 *   ...xxx1   an <integer>: the value shifted left one bit, 63 bits signed
 *   ...x010   a <character>: the Unicode code point shifted left three bits
 *   ...x000   a pointer to a heap or static object, 8-byte aligned
 *
 * (Tags 100 and 110 are unused.)  Every object a pointer points to begins
 * with a header word: a pointer to its class's WRAPPER, which holds what
 * the runtime needs to know of the class -- the class object itself, the
 * class's id, the instance size and representation, and the ancestor set
 * that instance? tests.  #t, #f and () are static objects, compared by
 * address.
 *
 * The compiler emits every class's wrapper and class object as static data
 * (compiler/backend/cdata.lisp), along with the program's symbols.  The
 * runtime refers to the built-in classes it needs by the names declared
 * below.
 *
 * Memory is the Boehm-Demers-Weiser collector, built for threads; objects
 * that hold no pointers (strings, floats, ratios) are allocated atomic so
 * the collector does not scan them.  This file must be included before
 * <pthread.h> so that threads are created through the collector.
 *
 * C11 and POSIX (threads, open_memstream).  The extensions of GCC and
 * Clang it uses are behind the macros of "Compilers" below, which fall
 * back on standard C where they can; musttail, which C has no substitute
 * for, is needed unless DY_TRAMPOLINE is defined.  Beyond C11, static data
 * is initialized with pointers converted to D, which C allows compilers to
 * accept and the usual ones do.
 * Define DY_DEBUG to check the representation of each value the
 * compiler's inline code takes apart (CompilerPlan.md, Phase 10).
 */

#ifndef DYLAN_H
#define DYLAN_H

/* POSIX, which -std=c11 hides */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 700
#endif

#define GC_THREADS 1
#include <gc.h>

/* With DY_GC_STRESS, a collection every DYLAN_GC_STRESS (an environment
 * variable, default 100) allocations, to find what the collector does not
 * see (CompilerPlan.md, Phase 11). */
void dy_gc_stress(void);
#ifdef DY_GC_STRESS
#undef GC_MALLOC
#undef GC_MALLOC_ATOMIC
#undef GC_MALLOC_UNCOLLECTABLE
#define GC_MALLOC(n) (dy_gc_stress(), GC_malloc(n))
#define GC_MALLOC_ATOMIC(n) (dy_gc_stress(), GC_malloc_atomic(n))
#define GC_MALLOC_UNCOLLECTABLE(n) (dy_gc_stress(), GC_malloc_uncollectable(n))
#endif
#include <pthread.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#if UINTPTR_MAX != 0xFFFFFFFFFFFFFFFFu
#error "The Dylan runtime needs 64-bit pointers."
#endif

typedef uintptr_t D;

_Static_assert(sizeof(D) == 8 && sizeof(void *) == 8, "a D is a 64-bit word");
_Static_assert(sizeof(double) == 8, "<double-float> is a 64-bit double");
_Static_assert((intptr_t)-1 >> 1 == -1, "right shifts of integers are arithmetic");

/* The version of what the compiler's C and the runtime agree on; the
 * compiler's C checks it. */
#define DY_ABI_VERSION 13

/* ------------------------------------------------------------ Compilers */

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#if defined(__GNUC__) || defined(__clang__)
#define DY_LIKELY(c)   __builtin_expect(!!(c), 1)
#define DY_UNLIKELY(c) __builtin_expect(!!(c), 0)
#else
#define DY_LIKELY(c)   (c)
#define DY_UNLIKELY(c) (c)
#endif

/* Set *R to A op B, answering whether that overflowed. */
#if __has_builtin(__builtin_add_overflow) || (defined(__GNUC__) && __GNUC__ >= 5)
#define DY_ADD_OVERFLOW(a, b, r) __builtin_add_overflow((a), (b), (r))
#define DY_SUB_OVERFLOW(a, b, r) __builtin_sub_overflow((a), (b), (r))
#define DY_MUL_OVERFLOW(a, b, r) __builtin_mul_overflow((a), (b), (r))
#else
static inline bool dy_add_overflow(intptr_t a, intptr_t b, intptr_t *r) {
    if ((b > 0 && a > INTPTR_MAX - b) || (b < 0 && a < INTPTR_MIN - b)) return true;
    *r = a + b;
    return false;
}
static inline bool dy_sub_overflow(intptr_t a, intptr_t b, intptr_t *r) {
    if ((b < 0 && a > INTPTR_MAX + b) || (b > 0 && a < INTPTR_MIN + b)) return true;
    *r = a - b;
    return false;
}
static inline bool dy_mul_overflow(intptr_t a, intptr_t b, intptr_t *r) {
    if (a != 0 && b != 0) {
        if (a == -1 && b == INTPTR_MIN) return true;
        if (b == -1 && a == INTPTR_MIN) return true;
        if (a > 0 ? (b > 0 ? a > INTPTR_MAX / b : b < INTPTR_MIN / a)
                  : (b > 0 ? a < INTPTR_MIN / b : a < INTPTR_MAX / b))
            return true;
    }
    *r = a * b;
    return false;
}
#define DY_ADD_OVERFLOW(a, b, r) dy_add_overflow((a), (b), (r))
#define DY_SUB_OVERFLOW(a, b, r) dy_sub_overflow((a), (b), (r))
#define DY_MUL_OVERFLOW(a, b, r) dy_mul_overflow((a), (b), (r))
#endif

/* A tail call that must be a jump.  Without it, build with
 * -DDY_TRAMPOLINE. */
#ifndef DY_TRAMPOLINE
#if __has_attribute(musttail)
#define DY_MUSTTAIL __attribute__((musttail))
#else
#error "This C compiler cannot guarantee tail calls (musttail): define DY_TRAMPOLINE."
#endif
#endif

/* ---------------------------------------------------------------- Tags */

#define DY_TAG_MASK        ((D)7)
#define DY_TAG_POINTER     ((D)0)
#define DY_TAG_CHARACTER   ((D)2)

#define DY_IS_INT(x)       (((D)(x) & 1) != 0)
#define DY_IS_CHAR(x)      (((D)(x) & DY_TAG_MASK) == DY_TAG_CHARACTER)
#define DY_IS_POINTER(x)   (((D)(x) & DY_TAG_MASK) == DY_TAG_POINTER)

/* Integers: 63 bits, from DY_INT_MIN to DY_INT_MAX. */
#define DY_INT_MAX         ((intptr_t)(((uintptr_t)1 << 62) - 1))
#define DY_INT_MIN         (-DY_INT_MAX - 1)
#define DY_INT(n)          ((D)(((uintptr_t)(intptr_t)(n) << 1) | 1))
#define DY_INT_VALUE(x)    ((intptr_t)(x) >> 1)

/* Characters: Unicode code points. */
#define DY_CHAR(c)         ((D)(((uintptr_t)(c) << 3) | DY_TAG_CHARACTER))
#define DY_CHAR_CODE(x)    ((uint32_t)((D)(x) >> 3))

#define DY_PTR(x)          ((void *)(x))
#define DY_OBJ(p)          ((D)(p))

/* ------------------------------------------------------------- Classes */

/* How a class's instances are represented.  Kept in step with the kinds
 * of compiler/backend/classes.lisp. */
enum {
    DY_KIND_NONE = 0,              /* no direct instances (yet)        */
    DY_KIND_INSTANCE,              /* header and DY_WRAPPER.size slots  */
    DY_KIND_INTEGER,               /* immediate                         */
    DY_KIND_CHARACTER,             /* immediate                         */
    DY_KIND_BOOLEAN,
    DY_KIND_EMPTY_LIST,
    DY_KIND_PAIR,
    DY_KIND_RATIO,
    DY_KIND_SINGLE_FLOAT,
    DY_KIND_DOUBLE_FLOAT,
    DY_KIND_EXTENDED_FLOAT,
    DY_KIND_BYTE_STRING,
    DY_KIND_UNICODE_STRING,
    DY_KIND_SIMPLE_OBJECT_VECTOR,
    DY_KIND_SYMBOL,
    DY_KIND_KEYWORD,
    DY_KIND_CLASS,
    DY_KIND_FUNCTION,              /* DyFunction                        */
    DY_KIND_GENERIC,               /* DyGeneric                         */
    DY_KIND_SINGLETON,             /* DySingleton                       */
    DY_KIND_THREAD,                /* DyThreadObject                    */
    DY_KIND_LOCK,                  /* DyLock: <lock>, <recursive-lock>  */
    DY_KIND_CONDITION_VARIABLE,    /* DyConditionVariable               */
    DY_KIND_ATOMIC_INTEGER,        /* DyAtomicInteger                   */
    DY_KIND_SLOT_DESCRIPTOR        /* DySlotDescriptor                  */
};

/* Wrapper flags */
#define DY_WRAPPER_RANGE_OK   1u    /* instance? is a range check       */
#define DY_WRAPPER_ABSTRACT   2u
#define DY_WRAPPER_SEALED     4u

typedef struct DyWrapper {
    D class;                       /* the <class> object                 */
    uint32_t id;                   /* preorder number                    */
    uint32_t subtree_end;          /* last id of its first-superclass
                                      subtree                            */
    uint32_t size;                 /* DY_KIND_INSTANCE: number of slots  */
    uint16_t kind;
    uint16_t flags;
    const uint8_t *ancestors;      /* bit i set: class i is an ancestor
                                      (or the class itself)              */
    const int32_t *slot_offsets;   /* index of each of the program's
                                      instance slots in DyInstance.slots,
                                      or -1; NULL if it has none          */
} DyWrapper;

/* Every heap and static object begins with this. */
typedef struct DyHeader {
    DyWrapper *wrapper;
} DyHeader;

typedef struct DyInstance {
    DyWrapper *wrapper;
    D slots[];
} DyInstance;

typedef struct DyClass {
    DyWrapper *wrapper;            /* <class>'s                          */
    DyWrapper *instance_wrapper;   /* the wrapper of its instances       */
    D name;                        /* a <symbol>                         */
    D direct_superclasses;         /* <simple-object-vector>             */
    D precedence_list;             /* <simple-object-vector>, self first */
    D direct_subclasses;           /* <simple-object-vector>             */
    D (*maker)(D self, D a0, D a1, D a2, D a3, intptr_t argc);
                                   /* make for the class, or NULL        */
    int sealed;                    /* has seal sealed it?                */
} DyClass;

/* ------------------------------------------------------------- Objects */

typedef struct DyPair {
    DyWrapper *wrapper;
    D head;
    D tail;
} DyPair;

typedef struct DyRatio {           /* lowest terms, denominator > 1     */
    DyWrapper *wrapper;
    D numerator;                   /* <integer>s                         */
    D denominator;
} DyRatio;

typedef struct DySingleFloat {
    DyWrapper *wrapper;
    float value;
} DySingleFloat;

typedef struct DyDoubleFloat {     /* <double-float> and <extended-float> */
    DyWrapper *wrapper;
    double value;
} DyDoubleFloat;

typedef struct DyByteString {
    DyWrapper *wrapper;
    D size;                        /* an <integer>                       */
    char data[];                   /* SIZE bytes, then a NUL for C's sake */
} DyByteString;

typedef struct DyUnicodeString {
    DyWrapper *wrapper;
    D size;
    uint32_t data[];
} DyUnicodeString;

typedef struct DyVector {          /* <simple-object-vector>             */
    DyWrapper *wrapper;
    D size;
    D data[];
} DyVector;

typedef struct DySymbol {          /* <symbol> and <keyword>             */
    DyWrapper *wrapper;
    D name;                        /* <byte-string>, as first spelled    */
    uint64_t hash;                 /* of the case-folded name; 0 until
                                      interned                           */
} DySymbol;

/* The static objects */
extern DyHeader dy_true_object, dy_false_object, dy_empty_list_object;
#define DY_TRUE   DY_OBJ(&dy_true_object)
#define DY_FALSE  DY_OBJ(&dy_false_object)
#define DY_EMPTY  DY_OBJ(&dy_empty_list_object)
#define DY_BOOL(c) ((c) ? DY_TRUE : DY_FALSE)

/* The built-in classes the runtime itself refers to.  Defined by the
 * compiler's output. */
extern DyWrapper dy_wrapper_object, dy_wrapper_boolean, dy_wrapper_integer,
    dy_wrapper_character, dy_wrapper_empty_list, dy_wrapper_pair,
    dy_wrapper_ratio, dy_wrapper_single_float, dy_wrapper_double_float,
    dy_wrapper_extended_float, dy_wrapper_byte_string,
    dy_wrapper_unicode_string, dy_wrapper_simple_object_vector,
    dy_wrapper_symbol, dy_wrapper_keyword, dy_wrapper_class,
    dy_wrapper_number, dy_wrapper_rational, dy_wrapper_float,
    dy_wrapper_real, dy_wrapper_method, dy_wrapper_generic_function,
    dy_wrapper_singleton, dy_wrapper_thread, dy_wrapper_lock,
    dy_wrapper_recursive_lock, dy_wrapper_condition_variable,
    dy_wrapper_atomic_integer, dy_wrapper_slot_descriptor;

/* The program's symbols and keywords, which the runtime interns when it
 * starts.  Defined by the compiler's output. */
extern DySymbol *const dy_static_symbols[];
extern const size_t dy_static_symbol_count;

/* Every class of the program, in id order. */
extern DyClass *const dy_all_classes[];
extern const size_t dy_class_count;

/* ------------------------------------------------- Startup and errors */

void dy_init(void);

/* Report a run-time error and stop.  (Phase 6 makes these signal Dylan
 * conditions.)  IRRITANT is shown after the message unless it is 0. */
_Noreturn void dy_error(const char *message, D irritant);
_Noreturn void dy_integer_overflow(void);
_Noreturn void dy_division_by_zero(void);

/* ---------------------------------------------------------- Allocation */

static inline void *dy_alloc(DyWrapper *wrapper, size_t bytes) {
    DyHeader *object = GC_MALLOC(bytes);
    if (!object) dy_error("out of memory", 0);
    object->wrapper = wrapper;
    return object;
}

/* For objects holding no pointers to collected memory.  The collector
 * does not clear these, so every byte must be set. */
static inline void *dy_alloc_atomic(DyWrapper *wrapper, size_t bytes) {
    DyHeader *object = GC_MALLOC_ATOMIC(bytes);
    if (!object) dy_error("out of memory", 0);
    object->wrapper = wrapper;
    return object;
}

/* ------------------------------------------------- Classes of objects */

static inline DyWrapper *dy_wrapper_of(D x) {
    if (DY_IS_INT(x)) return &dy_wrapper_integer;
    if (DY_IS_CHAR(x)) return &dy_wrapper_character;
    return ((DyHeader *)DY_PTR(x))->wrapper;
}

static inline D dy_object_class(D x) { return dy_wrapper_of(x)->class; }

static inline bool dy_wrapper_subtype(const DyWrapper *w, const DyWrapper *c) {
    if (c->flags & DY_WRAPPER_RANGE_OK)
        return (uint32_t)(w->id - c->id) <= (uint32_t)(c->subtree_end - c->id);
    return (w->ancestors[c->id >> 3] >> (c->id & 7)) & 1;
}

/* Is X an instance of the class whose instances have wrapper C? */
static inline bool dy_instance_p(D x, const DyWrapper *c) {
    return dy_wrapper_subtype(dy_wrapper_of(x), c);
}

static inline bool dy_kind_p(D x, unsigned kind) {
    return DY_IS_POINTER(x) && ((DyHeader *)DY_PTR(x))->wrapper->kind == kind;
}

/* Accessors for the compiler's inline fast paths */
#define DY_IS_DOUBLE(x)    dy_kind_p((x), DY_KIND_DOUBLE_FLOAT)
#define DY_DOUBLE(x)       dy_double_value(x)
#define DY_VECTOR_SIZE(v)  dy_vector_size_of(v)
#define DY_VECTOR_DATA(v)  dy_vector_data_of(v)
#define DY_STRING_SIZE(s)  dy_string_size_of(s)
#define DY_STRING_DATA(s)  dy_string_data_of(s)
/* (Functions, not casts in place, so that C compilers do not warn of
 * type punning when the object is a static constant.) */
/* With DY_DEBUG, each checks that its argument is what it takes apart. */
_Noreturn void dy_representation_error(const char *expected, D x);
#ifdef DY_DEBUG
#define DY_REPRESENTATION(c, expected, x) \
    ((c) ? (void)0 : dy_representation_error((expected), (x)))
#else
#define DY_REPRESENTATION(c, expected, x) ((void)0)
#endif
static inline double dy_double_value(D x) {
    DY_REPRESENTATION(dy_kind_p(x, DY_KIND_DOUBLE_FLOAT), "a <double-float>", x);
    return ((DyDoubleFloat *)DY_PTR(x))->value;
}
static inline intptr_t dy_vector_size_of(D v) {
    DY_REPRESENTATION(dy_kind_p(v, DY_KIND_SIMPLE_OBJECT_VECTOR), "a <simple-object-vector>", v);
    return DY_INT_VALUE(((DyVector *)DY_PTR(v))->size);
}
static inline D *dy_vector_data_of(D v) {
    DY_REPRESENTATION(dy_kind_p(v, DY_KIND_SIMPLE_OBJECT_VECTOR), "a <simple-object-vector>", v);
    return ((DyVector *)DY_PTR(v))->data;
}
static inline intptr_t dy_string_size_of(D s) {
    DY_REPRESENTATION(dy_kind_p(s, DY_KIND_BYTE_STRING), "a <byte-string>", s);
    return DY_INT_VALUE(((DyByteString *)DY_PTR(s))->size);
}
static inline char *dy_string_data_of(D s) {
    DY_REPRESENTATION(dy_kind_p(s, DY_KIND_BYTE_STRING), "a <byte-string>", s);
    return ((DyByteString *)DY_PTR(s))->data;
}

/* The place of the program's instance slot number K in X, or NULL if X
 * has no such slot */
static inline D *dy_instance_slot(D x, int32_t k) {
    if (!DY_IS_POINTER(x)) return NULL;
    const int32_t *o = ((DyHeader *)DY_PTR(x))->wrapper->slot_offsets;
    if (!o || o[k] < 0) return NULL;
    return &((DyInstance *)DY_PTR(x))->slots[o[k]];
}

/* ------------------------------------------------------------ Integers
 *
 * On the tagged words directly: a = 2x+1 and b = 2y+1.  Any result that
 * does not fit in 63 bits is an error. */

static inline D dy_int_add(D a, D b) {
    intptr_t r;
    if (DY_UNLIKELY(DY_ADD_OVERFLOW((intptr_t)a, (intptr_t)b - 1, &r)))
        dy_integer_overflow();
    return (D)r;
}

static inline D dy_int_sub(D a, D b) {
    intptr_t r;
    if (DY_UNLIKELY(DY_SUB_OVERFLOW((intptr_t)a, (intptr_t)b - 1, &r)))
        dy_integer_overflow();
    return (D)r;
}

static inline D dy_int_mul(D a, D b) {
    intptr_t r;
    if (DY_UNLIKELY(DY_MUL_OVERFLOW(DY_INT_VALUE(a), (intptr_t)b - 1, &r)))
        dy_integer_overflow();
    return (D)r | 1;
}

static inline D dy_int_negate(D a) {
    intptr_t r;
    if (DY_UNLIKELY(DY_SUB_OVERFLOW((intptr_t)2, (intptr_t)a, &r)))
        dy_integer_overflow();
    return (D)r;
}

static inline bool dy_int_less(D a, D b) { return (intptr_t)a < (intptr_t)b; }

/* The remainder of floor division, of a divisor not 0: the sign of B. */
static inline intptr_t dy_int_modulo(intptr_t a, intptr_t b) {
    intptr_t r = a % b;
    return r != 0 && ((r < 0) != (b < 0)) ? r + b : r;
}

/* Make an <integer> of a C integer, checking its range. */
static inline D dy_make_int(intptr_t n) {
    if (n < DY_INT_MIN || n > DY_INT_MAX) dy_integer_overflow();
    return DY_INT(n);
}

/* Division of integers, as chapter 14's truncate/, floor/, ceiling/ and
 * round/: the quotient is answered and the remainder stored. */
D dy_int_truncate(D a, D b, D *remainder);
D dy_int_floor(D a, D b, D *remainder);
D dy_int_ceiling(D a, D b, D *remainder);
D dy_int_round(D a, D b, D *remainder);

/* ------------------------------------------------------------- Numbers
 *
 * Arithmetic on any two numbers, with Common Lisp's contagion (chapter
 * 14): float contagion, rational canonicalization, and exact (rational)
 * comparison of floats with rationals.  A single-float with a double-float
 * is a double-float; either with an extended-float is an extended-float. */

bool dy_number_p(D x);
D dy_make_ratio(D numerator, D denominator);    /* canonicalized */
D dy_make_single_float(float value);
D dy_make_double_float(double value);
D dy_make_extended_float(double value);
double dy_float_value(D x);                     /* any float */

D dy_add(D a, D b);
D dy_sub(D a, D b);
D dy_mul(D a, D b);
D dy_div(D a, D b);                             /* exact for rationals */
D dy_negate(D a);
int dy_compare(D a, D b);                       /* -1, 0, 1; 2 if a NaN */
bool dy_number_equal(D a, D b);                 /* = */
bool dy_number_less(D a, D b);                  /* < */

/* ----------------------------------------------- Strings and symbols */

D dy_make_byte_string(const char *bytes, size_t size);
D dy_make_c_string(const char *s);
D dy_make_unicode_string(const uint32_t *chars, size_t size);
D dy_make_vector(size_t size, D fill);
D dy_pair(D head, D tail);

/* Symbols and keywords are not case sensitive; the case of the first
 * spelling is kept (chapter 15).  These answer the one symbol (keyword) of
 * that name, making it if need be.  Safe to call from any thread. */
D dy_intern_symbol(const char *name, size_t size);
D dy_intern_keyword(const char *name, size_t size);

/* ---------------------------------------------------------- Functions
 *
 * Every function is called through its uniform entry (CompilerPlan.md,
 * Phase 4):
 *
 *     D entry(D self, D a0, D a1, D a2, D a3, intptr_t argc)
 *
 * SELF is the function object called.  The first four arguments are
 * A0..A3 (unused ones are 0); argument i >= 4 is in the thread's argument
 * area, dy_thread->args[i - 4], which the callee reads before it calls
 * anything.  One prototype for every function is what lets a tail call
 * be compiled as a jump (musttail).
 *
 * A function returns its first value.  The number of values, and all of
 * them when there is not exactly one, are in dy_thread->mv_count and
 * dy_thread->mv.  A caller that wants every value sets mv_count to 1
 * before the call; a function returns a single value without touching
 * mv_count, except that one that has made a call (which may have left
 * another count there) sets it to 1 first.  values sets it. */

typedef D (*DyEntry)(D self, D a0, D a1, D a2, D a3, intptr_t argc);

typedef struct DyFunction {        /* <method>                          */
    DyWrapper *wrapper;
    DyEntry entry;
    D specializers;                /* <simple-object-vector>, or #f when
                                      every one is <object> or unknown   */
    D info;                        /* #(required rest? keys) for
                                      function-arguments, keys #f for no
                                      #key; 0 or #f if unknown           */
    D env[];                       /* a closure's free variables         */
} DyFunction;

typedef struct DyGeneric {         /* <generic-function>                */
    DyWrapper *wrapper;
    DyEntry entry;                 /* its dispatch function              */
    D name;                        /* <symbol>                           */
    D methods;                     /* <simple-object-vector>             */
    intptr_t required;
    D info;                        /* as DyFunction's                    */
} DyGeneric;

/* A slot descriptor (chapter 11), one for each slot definition, made by
 * the compiler.  Its type, init_value and init_function are stored when
 * its define-class runs. */
typedef struct DySlotDescriptor {
    DyWrapper *wrapper;
    D getter, setter;              /* generic functions; setter or #f     */
    D type;                        /* DY_UNBOUND for <object>             */
    D allocation;                  /* a symbol                            */
    D init_value;                  /* DY_UNBOUND if none                  */
    D init_function;               /* DY_UNBOUND if none                  */
    D init_keyword;                /* keyword or #f                       */
    D owner;                       /* the class                           */
    intptr_t kind;                 /* 0 instance, 1 class, 2 each-subclass,
                                      3 constant, 4 virtual               */
    intptr_t number;               /* instance: the program's slot number */
    D *cell;                       /* class, constant: the storage;
                                      each-subclass: one per class id     */
} DySlotDescriptor;

typedef struct DySingleton {       /* (singleton object)                */
    DyWrapper *wrapper;
    D object;
} DySingleton;

#define DY_MAX_ARGUMENTS 1024
#define DY_MAX_VALUES    1024

typedef struct DyThread {
    intptr_t mv_count;
    D mv[DY_MAX_VALUES];
    D args[DY_MAX_ARGUMENTS - 4];  /* arguments 4 and up                 */
    /* The next method of the method being entered: set by the caller,
       read by the method before it calls anything. */
    D next_method;
    /* A pending tail call, in trampoline mode; TAIL_ENTRY, if not NULL,
       is the entry to use instead of TAIL_FUNCTION's own. */
    DyEntry tail_entry;
    D tail_function;
    D tail_a[4];
    intptr_t tail_argc;
    /* The dynamic chain: exits, cleanups and handlers, innermost first */
    struct DyFrame *frames;
    uint64_t frame_serial;
    int error_depth;               /* run-time errors being signalled    */
    /* Threads (Phase 7) */
    struct DyThreadObject *self;   /* this thread's <thread>             */
    struct DyFrame *top_frame;     /* where an unhandled condition ends
                                      a thread other than the main one   */
    D *thread_variables;           /* its values of the program's thread
                                      variables                          */
} DyThread;

/* ------------------------------------------------------------- Control
 * (CompilerPlan.md, Phase 6)
 *
 * The frames of the dynamic chain live on the C stack of the runtime
 * functions that establish them, so generated code never calls setjmp
 * and can always make tail calls.  bind-exit, when its exit leaves the
 * function, and unwind-protect are compiled as closures handed to these. */

enum { DY_FRAME_EXIT, DY_FRAME_CLEANUP, DY_FRAME_HANDLER };

typedef struct DyFrame {
    struct DyFrame *prev;
    int kind;
    int error_depth;               /* the thread's, when established     */
    uint64_t serial;               /* unique in the thread                */
    jmp_buf jump;                  /* DY_FRAME_EXIT                       */
    D a, b, c, d;                  /* CLEANUP: a the cleanup; HANDLER:
                                      type, function, test, description  */
} DyFrame;

/* Leave every frame above TARGET, running cleanups, and jump to it, with
 * the values the thread holds. */
_Noreturn void dy_unwind_to(DyFrame *target);

/* Call BODY, a function of one argument, with an exit procedure that
 * returns its arguments as the values of this call (bind-exit). */
D dy_call_with_exit(D body);
/* Call PROTECTED with no arguments, then CLEANUP, even if PROTECTED is
 * left by an exit; answer PROTECTED's values (unwind-protect). */
D dy_unwind_protect(D protected, D cleanup);
/* Call THUNK with a handler established (handler-bind). */
D dy_handler_bind(D type, D function, D test, D description, D thunk);

/* Each thread's state, allocated uncollectable so that the collector
 * scans it. */
extern _Thread_local DyThread *dy_thread;
void dy_init_thread(void);
void dy_end_thread(void);

/* Markers no Dylan program can see */
extern DyHeader dy_unbound_object, dy_unsupplied_object, dy_tail_call_object;
#define DY_UNBOUND     DY_OBJ(&dy_unbound_object)
#define DY_UNSUPPLIED  DY_OBJ(&dy_unsupplied_object)
#define DY_TAIL_MARKER DY_OBJ(&dy_tail_call_object)

_Noreturn void dy_not_a_function(D f);
_Noreturn void dy_wrong_argument_count(D f, intptr_t argc);
_Noreturn void dy_unbound_variable(const char *name);
_Noreturn void dy_odd_keyword_arguments(D f);
_Noreturn void dy_unknown_keyword(D f, D keyword);
_Noreturn void dy_too_many_arguments(void);

/* A module variable's value, which it must have. */
static inline D dy_check_bound(D value, const char *name) {
    if (DY_UNLIKELY(value == DY_UNBOUND)) dy_unbound_variable(name);
    return value;
}

/* VALUES[START..END) as a list. */
D dy_list_of(const D *values, intptr_t start, intptr_t end);

static inline bool dy_function_p(D f) {
    if (!DY_IS_POINTER(f)) return false;
    unsigned k = ((DyHeader *)DY_PTR(f))->wrapper->kind;
    return k == DY_KIND_FUNCTION || k == DY_KIND_GENERIC;
}

/* Both DyFunction and DyGeneric have the entry second. */
static inline DyEntry dy_entry(D f) {
    if (!dy_function_p(f)) dy_not_a_function(f);
    return ((DyFunction *)DY_PTR(f))->entry;
}

D dy_make_closure(DyEntry entry, size_t env_size);

/* Closures and instances on the stack of the function that makes them,
 * which the compiler has found do not outlive it (CompilerPlan.md, Phase
 * 9).  BUFFER is suitably aligned and large enough; the collector scans
 * the stack, so what they hold stays alive. */
static inline D dy_init_closure(void *buffer, DyEntry entry) {
    DyFunction *f = buffer;
    f->wrapper = &dy_wrapper_method;
    f->entry = entry;
    f->specializers = DY_FALSE;
    f->info = DY_FALSE;
    return DY_OBJ(f);
}

static inline D dy_init_instance(void *buffer, DyWrapper *wrapper) {
    DyInstance *x = buffer;
    x->wrapper = wrapper;
    for (uint32_t i = 0; i < wrapper->size; i++) x->slots[i] = DY_UNBOUND;
    return DY_OBJ(x);
}

/* A cell holds a variable that is both captured and assigned. */
static inline D dy_make_cell(D value) {
    D *cell = GC_MALLOC(sizeof(D));
    if (!cell) dy_error("out of memory", 0);
    *cell = value;
    return (D)cell;
}
#define DY_CELL(c) (*(D *)(c))

/* The arguments from index START on, as a list. */
D dy_collect_arguments(intptr_t argc, intptr_t start, D a0, D a1, D a2, D a3);

/* The same in PAIRS, an array of at least ARGC - START pairs on the
 * caller's stack, for a list the compiler has found never escapes. */
#define DY_STACK_PAIRS 8
D dy_collect_arguments_into(DyPair *pairs, intptr_t argc, intptr_t start,
                            D a0, D a1, D a2, D a3);

/* Argument I of a function's arguments A0..A3 and the argument area */
static inline D dy_argument(intptr_t i, D a0, D a1, D a2, D a3) {
    switch (i) {
    case 0: return a0;
    case 1: return a1;
    case 2: return a2;
    case 3: return a3;
    default: return dy_thread->args[i - 4];
    }
}

static inline D dy_head(D pair) {
    DY_REPRESENTATION(dy_kind_p(pair, DY_KIND_PAIR), "a <pair>", pair);
    return ((DyPair *)DY_PTR(pair))->head;
}
static inline D dy_tail(D pair) {
    DY_REPRESENTATION(dy_kind_p(pair, DY_KIND_PAIR), "a <pair>", pair);
    return ((DyPair *)DY_PTR(pair))->tail;
}

/* Type checks of bound values (bind's typed variables). */
void dy_check_type(D value, D type);

/* Calls.  In the default mode a tail call is a C tail call the compiler
 * must make a jump (musttail).  Built with -DDY_TRAMPOLINE, a tail call
 * instead stores the call in the thread and returns DY_TAIL_MARKER, and
 * every other call makes the pending calls until a real value comes back:
 * slower, but it needs nothing beyond standard C. */

#ifdef DY_TRAMPOLINE
static inline D dy_bounce(D r) {
    while (r == DY_TAIL_MARKER) {
        DyThread *t = dy_thread;
        D f = t->tail_function;
        DyEntry entry = t->tail_entry ? t->tail_entry : dy_entry(f);
        t->tail_entry = NULL;
        r = entry(f, t->tail_a[0], t->tail_a[1], t->tail_a[2],
                  t->tail_a[3], t->tail_argc);
    }
    return r;
}
#define DY_CALL(f, a0, a1, a2, a3, n) \
    dy_bounce(dy_entry(f)((f), (a0), (a1), (a2), (a3), (n)))
#define DY_CALL_DIRECT(fn, f, a0, a1, a2, a3, n) \
    dy_bounce(fn((f), (a0), (a1), (a2), (a3), (n)))
#define DY_TAIL_CALL(f, a0, a1, a2, a3, n) do {                          \
        DyThread *t_ = dy_thread;                                       \
        t_->tail_function = (f);                                        \
        t_->tail_a[0] = (a0); t_->tail_a[1] = (a1);                     \
        t_->tail_a[2] = (a2); t_->tail_a[3] = (a3);                     \
        t_->tail_argc = (n);                                            \
        return DY_TAIL_MARKER;                                          \
    } while (0)
#define DY_TAIL_CALL_DIRECT(fn, f, a0, a1, a2, a3, n) do {               \
        DyThread *t_ = dy_thread;                                       \
        t_->tail_entry = (fn);                                          \
        t_->tail_function = (f);                                        \
        t_->tail_a[0] = (a0); t_->tail_a[1] = (a1);                     \
        t_->tail_a[2] = (a2); t_->tail_a[3] = (a3);                     \
        t_->tail_argc = (n);                                            \
        return DY_TAIL_MARKER;                                          \
    } while (0)
#else
#define DY_CALL(f, a0, a1, a2, a3, n) \
    dy_entry(f)((f), (a0), (a1), (a2), (a3), (n))
#define DY_CALL_DIRECT(fn, f, a0, a1, a2, a3, n) \
    fn((f), (a0), (a1), (a2), (a3), (n))
#define DY_TAIL_CALL(f, a0, a1, a2, a3, n) \
    DY_MUSTTAIL return dy_entry(f)((f), (a0), (a1), (a2), (a3), (n))
#define DY_TAIL_CALL_DIRECT(fn, f, a0, a1, a2, a3, n) \
    DY_MUSTTAIL return fn((f), (a0), (a1), (a2), (a3), (n))
#endif

/* -------------------------------------------------------- Object system
 * (CompilerPlan.md, Phase 5) */

/* id? : identity, except that numbers of one class and value are id?. */
bool dy_identical(D x, D y);

/* Does X match the specializer SPEC, a class or a singleton? */
bool dy_specializer_matches(D x, D spec);

_Noreturn void dy_no_applicable_method(D generic);
_Noreturn void dy_unbound_slot(const char *getter);
_Noreturn void dy_not_a_slot(const char *getter, D instance);

/* Check the arguments from index START: keyword/value pairs whose
 * keywords are among the N in ACCEPTED (any, if ACCEPTED is NULL). */
void dy_check_keywords(D f, intptr_t argc, intptr_t start, D a0, D a1, D a2,
                       D a3, const D *accepted, size_t n);

/* A next-method object that, called with no arguments, calls NEXT with
 * the arguments the current method was given. */
D dy_capture_next(D next, intptr_t argc, D a0, D a1, D a2, D a3);

/* A new instance of the class whose instances have WRAPPER, every slot
 * uninitialized (DY_UNBOUND). */
D dy_make_instance(DyWrapper *wrapper);

/* The value after KEYWORD in the list of keyword/value pairs PAIRS (the
 * leftmost), or DY_UNSUPPLIED. */
D dy_find_keyword(D pairs, D keyword);

/* Call F with FIRST and the elements of LIST as its arguments. */
D dy_call_list(D f, D first, D list);

/* initialize's method on <object>: does nothing. */
extern DyFunction dy_default_initialize;

/* ------------------------------------------------------------- Threads
 * (CompilerPlan.md, Phase 7: an extension; the manual has no threads) */

typedef struct DyThreadObject {    /* <thread>                          */
    DyWrapper *wrapper;
    pthread_t thread;
    D function;
    D results;                     /* the function's values, as a list    */
    D condition;                   /* the condition that ended it, or #f  */
    int done;
    int main;
    pthread_mutex_t lock;          /* with ENDED, for join-thread: the    */
    pthread_cond_t ended;          /* thread is detached                  */
} DyThreadObject;

typedef struct DyLock {            /* <lock> and <recursive-lock>       */
    DyWrapper *wrapper;
    pthread_mutex_t mutex;
    pthread_t owner;               /* the thread holding it, while DEPTH */
    long depth;                    /* how many times it holds it         */
} DyLock;

typedef struct DyConditionVariable {
    DyWrapper *wrapper;
    pthread_cond_t condition;
} DyConditionVariable;

typedef struct DyAtomicInteger {
    DyWrapper *wrapper;
    _Atomic intptr_t value;        /* untagged                           */
} DyAtomicInteger;

/* The program's thread variables: their number and the values new threads
 * start with.  Defined by the compiler's output. */
extern const size_t dy_thread_variable_count;
extern D dy_thread_variable_defaults[];

/* Runtime support for backquote */
D dy_append2(D list, D tail);
D dy_list_to_vector(D list);

/* ------------------------------------------------------------- Debug */

/* Write X to stdout roughly as Dylan would, for tests and debugging (the
 * library's printer comes in Phase 8). */
void dy_debug_print(D x);
/* A number as the library prints it (runtime/c/library.c) */
D dy_number_string(D x);
void dy_debug_write(FILE *out, D x);

#endif
