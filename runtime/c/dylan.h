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
 * Requires GCC or Clang (64-bit): the overflow builtins, __int128, and the
 * acceptance of pointers converted to D in static initializers.
 */

#ifndef DYLAN_H
#define DYLAN_H

#define GC_THREADS 1
#include <gc.h>
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
    DY_KIND_ATOMIC_INTEGER         /* DyAtomicInteger                   */
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
    dy_wrapper_atomic_integer;

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

/* ------------------------------------------------------------ Integers
 *
 * On the tagged words directly: a = 2x+1 and b = 2y+1.  Any result that
 * does not fit in 63 bits is an error. */

static inline D dy_int_add(D a, D b) {
    intptr_t r;
    if (__builtin_add_overflow((intptr_t)a, (intptr_t)b - 1, &r))
        dy_integer_overflow();
    return (D)r;
}

static inline D dy_int_sub(D a, D b) {
    intptr_t r;
    if (__builtin_sub_overflow((intptr_t)a, (intptr_t)b - 1, &r))
        dy_integer_overflow();
    return (D)r;
}

static inline D dy_int_mul(D a, D b) {
    intptr_t r;
    if (__builtin_mul_overflow(DY_INT_VALUE(a), (intptr_t)b - 1, &r))
        dy_integer_overflow();
    return (D)r | 1;
}

static inline D dy_int_negate(D a) {
    intptr_t r;
    if (__builtin_sub_overflow((intptr_t)2, (intptr_t)a, &r))
        dy_integer_overflow();
    return (D)r;
}

static inline bool dy_int_less(D a, D b) { return (intptr_t)a < (intptr_t)b; }

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
    D env[];                       /* a closure's free variables         */
} DyFunction;

typedef struct DyGeneric {         /* <generic-function>                */
    DyWrapper *wrapper;
    DyEntry entry;                 /* its dispatch function              */
    D name;                        /* <symbol>                           */
    D methods;                     /* <simple-object-vector>             */
    intptr_t required;
} DyGeneric;

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
    if (value == DY_UNBOUND) dy_unbound_variable(name);
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

static inline D dy_head(D pair) { return ((DyPair *)DY_PTR(pair))->head; }
static inline D dy_tail(D pair) { return ((DyPair *)DY_PTR(pair))->tail; }

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
    __attribute__((musttail)) return dy_entry(f)((f), (a0), (a1), (a2), (a3), (n))
#define DY_TAIL_CALL_DIRECT(fn, f, a0, a1, a2, a3, n) \
    __attribute__((musttail)) return fn((f), (a0), (a1), (a2), (a3), (n))
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
} DyThreadObject;

typedef struct DyLock {            /* <lock> and <recursive-lock>       */
    DyWrapper *wrapper;
    pthread_mutex_t mutex;
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
void dy_debug_write(FILE *out, D x);

#endif
