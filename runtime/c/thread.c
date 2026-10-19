/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Threads, locks, condition variables and atomic integers
 * (CompilerPlan.md, Phase 7).  The 1992 manual has no threads; these are
 * an extension.  Each <thread> is a POSIX thread, created through the
 * collector (dylan.h includes gc.h with GC_THREADS before pthread.h), with
 * its own DyThread state.  A thread runs its function under an exit frame:
 * a serious condition it does not handle ends the thread, not the
 * program, and is kept for join-thread, which the library writes in Dylan
 * (runtime/dylan/threads.dyl) on the primitives here.
 */

#include "primitive.h"
#include <errno.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>

static DyThreadObject *new_thread_object(D function) {
    DyThreadObject *t = dy_alloc_fixed(&dy_wrapper_thread, sizeof(DyThreadObject));
    t->function = function;
    t->results = DY_EMPTY;
    t->condition = DY_FALSE;
    t->done = 0;
    t->main = 0;
#ifdef DY_MPS
    t->sync = dy_mps_make_thread_sync();
#endif
    pthread_mutex_init(DY_THREAD_LOCK(t), NULL);
    pthread_cond_init(DY_THREAD_ENDED(t), NULL);
    return t;
}

/* The main thread's <thread>; made by dy_init. */
void dy_init_main_thread(void) {
    DyThreadObject *t = new_thread_object(DY_FALSE);
    t->thread = pthread_self();
    t->main = 1;
    dy_thread->self = t;
}

static void *thread_main(void *arg) {
    DyThreadObject *t = arg;
    dy_init_thread();
    dy_thread->self = t;
    DyFrame top;
    top.kind = DY_FRAME_EXIT;
    top.prev = NULL;
    top.serial = ++dy_thread->frame_serial;
    top.error_depth = 0;
    dy_thread->frames = &top;
    dy_thread->top_frame = &top;
    if (_setjmp(top.jump) == 0) {
        dy_thread->mv_count = 1;
        D first = DY_CALL(t->function, 0, 0, 0, 0, 0);
        intptr_t n = dy_thread->mv_count;
        t->results = n == 1 ? dy_pair(first, DY_EMPTY) : dy_list_of(dy_thread->mv, 0, n);
    }
    /* Otherwise an unhandled condition ended it; %unhandled kept it. */
    dy_thread->frames = NULL;
    pthread_mutex_lock(DY_THREAD_LOCK(t));
    t->done = 1;
    pthread_cond_broadcast(DY_THREAD_ENDED(t));
    pthread_mutex_unlock(DY_THREAD_LOCK(t));
    dy_end_thread();
    return NULL;
}

/* (%make-thread function): start a thread calling FUNCTION. */
PRIMITIVE(make_thread) {
    need(self, argc, 1, 1);
    if (!dy_function_p(a0)) dy_error("make-thread: not a function", a0);
    DyThreadObject *t = new_thread_object(a0);
    int status = pthread_create(&t->thread, NULL, thread_main, t);
    if (status != 0) dy_error("make-thread: the thread could not be created", 0);
    /* join-thread waits on ENDED, so no thread need be joined. */
    pthread_detach(t->thread);
    return DY_OBJ(t);
}

static DyThreadObject *as_thread(D x) {
    if (!dy_kind_p(x, DY_KIND_THREAD)) dy_error("not a thread", x);
    return DY_PTR(x);
}

/* (%join-thread thread): wait for THREAD to end; answer its function's
 * values as a list (or () if a condition ended it). */
PRIMITIVE(join_thread) {
    need(self, argc, 1, 1);
    DyThreadObject *t = as_thread(a0);
    if (t == dy_thread->self) dy_error("join-thread: a thread cannot join itself", a0);
    if (t->main) dy_error("join-thread: the main thread cannot be joined", a0);
    pthread_mutex_lock(DY_THREAD_LOCK(t));
    while (!t->done) pthread_cond_wait(DY_THREAD_ENDED(t), DY_THREAD_LOCK(t));
    pthread_mutex_unlock(DY_THREAD_LOCK(t));
    return t->results;
}

/* (%thread-condition thread): the condition that ended THREAD, or #f. */
PRIMITIVE(thread_condition) {
    need(self, argc, 1, 1);
    return as_thread(a0)->condition;
}

PRIMITIVE(current_thread) {
    need(self, argc, 0, 0);
    return DY_OBJ(dy_thread->self);
}

PRIMITIVE(thread_yield) {
    need(self, argc, 0, 0);
    sched_yield();
    return DY_FALSE;
}

/* (%unhandled condition message): what default-handler does with a serious
 * condition no handler took.  In the main thread, report MESSAGE and stop
 * the program; in any other, end the thread, keeping CONDITION. */
PRIMITIVE(unhandled) {
    need(self, argc, 2, 2);
    DyThread *t = dy_thread;
    if (t->top_frame) {
        t->self->condition = a0;
        t->mv_count = 0;
        dy_unwind_to(t->top_frame);
    }
    fflush(stdout);
    fputs("Dylan error: ", stderr);
    if (dy_kind_p(a1, DY_KIND_BYTE_STRING)) {
        DyByteString *s = DY_PTR(a1);
        fwrite(s->data, 1, DY_INT_VALUE(s->size), stderr);
    } else {
        dy_debug_write(stderr, a1);
    }
    fputc('\n', stderr);
    exit(1);
}

/* ---------------------------------------------------------------- Locks */

static D make_lock(D self, intptr_t argc, DyWrapper *wrapper, int type) {
    if (argc != 1) dy_error("make: a lock takes no init-keywords", self);
    DyLock *l = dy_alloc_fixed_atomic(wrapper, sizeof(DyLock));
    pthread_mutexattr_t attributes;
    pthread_mutexattr_init(&attributes);
    pthread_mutexattr_settype(&attributes, type);
    pthread_mutex_init(&l->mutex, &attributes);
    pthread_mutexattr_destroy(&attributes);
    l->depth = 0;
    return DY_OBJ(l);
}

D dy_make_lock(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    (void)a0; (void)a1; (void)a2; (void)a3;
    return make_lock(self, argc, &dy_wrapper_lock, PTHREAD_MUTEX_ERRORCHECK);
}

D dy_make_recursive_lock(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    (void)a0; (void)a1; (void)a2; (void)a3;
    return make_lock(self, argc, &dy_wrapper_recursive_lock, PTHREAD_MUTEX_RECURSIVE);
}

static DyLock *as_lock(D x) {
    if (!dy_kind_p(x, DY_KIND_LOCK)) dy_error("not a lock", x);
    return DY_PTR(x);
}

/* Does this thread hold L?  (Its owner and depth change only while it
 * is held, so only the holder can find itself.) */
static bool held(DyLock *l) {
    return __atomic_load_n(&l->depth, __ATOMIC_RELAXED) > 0
        && pthread_equal(__atomic_load_n(&l->owner, __ATOMIC_RELAXED), pthread_self());
}

static void note_acquired(DyLock *l) {
    __atomic_store_n(&l->owner, pthread_self(), __ATOMIC_RELAXED);
    __atomic_store_n(&l->depth, l->depth + 1, __ATOMIC_RELAXED);
}

PRIMITIVE(acquire_lock) {
    need(self, argc, 1, 1);
    DyLock *l = as_lock(a0);
    if (dy_wrapper_of(a0) == &dy_wrapper_lock && held(l))
        dy_error("acquire-lock: the thread holds the lock already", a0);
    if (pthread_mutex_lock(&l->mutex) != 0)
        dy_error("acquire-lock: the lock could not be acquired", a0);
    note_acquired(l);
    return a0;
}

PRIMITIVE(release_lock) {
    need(self, argc, 1, 1);
    DyLock *l = as_lock(a0);
    /* Checked here, not by unlocking an unlocked mutex */
    if (!held(l)) dy_error("release-lock: the thread does not hold the lock", a0);
    __atomic_store_n(&l->depth, l->depth - 1, __ATOMIC_RELAXED);
    pthread_mutex_unlock(&l->mutex);
    return a0;
}

/* -------------------------------------------------- Condition variables */

D dy_make_condition_variable(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    (void)a0; (void)a1; (void)a2; (void)a3;
    if (argc != 1) dy_error("make: a condition variable takes no init-keywords", self);
    DyConditionVariable *c = dy_alloc_fixed_atomic(&dy_wrapper_condition_variable,
                                                   sizeof(DyConditionVariable));
    pthread_cond_init(&c->condition, NULL);
    return DY_OBJ(c);
}

static DyConditionVariable *as_condition_variable(D x) {
    if (!dy_kind_p(x, DY_KIND_CONDITION_VARIABLE)) dy_error("not a condition variable", x);
    return DY_PTR(x);
}

/* (wait-condition-variable cv lock): release LOCK, a <lock> the thread
 * holds, wait to be notified, and acquire it again.  It may also return
 * without a notification, so wait in a loop that tests what is awaited. */
PRIMITIVE(wait_condition_variable) {
    need(self, argc, 2, 2);
    DyConditionVariable *c = as_condition_variable(a0);
    if (!dy_kind_p(a1, DY_KIND_LOCK) || dy_wrapper_of(a1) != &dy_wrapper_lock)
        dy_error("wait-condition-variable: not a <lock>", a1);
    DyLock *l = as_lock(a1);
    if (!held(l)) dy_error("wait-condition-variable: the thread does not hold the lock", a1);
    __atomic_store_n(&l->depth, 0, __ATOMIC_RELAXED);
    pthread_cond_wait(&c->condition, &l->mutex);
    note_acquired(l);
    return DY_FALSE;
}

PRIMITIVE(notify_condition_variable) {
    need(self, argc, 1, 1);
    pthread_cond_signal(&as_condition_variable(a0)->condition);
    return DY_FALSE;
}

PRIMITIVE(notify_all_condition_variable) {
    need(self, argc, 1, 1);
    pthread_cond_broadcast(&as_condition_variable(a0)->condition);
    return DY_FALSE;
}

/* ------------------------------------------------------ Atomic integers */

/* (make <atomic-integer> value: n), n 0 if not given */
D dy_make_atomic_integer(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    (void)a0; (void)a3;
    D value = DY_INT(0);
    if (argc == 3 && a1 == dy_intern_keyword("value", 5)) value = a2;
    else if (argc != 1) dy_error("make: <atomic-integer> takes only value:", self);
    if (!DY_IS_INT(value)) dy_error("make: value: must be an integer", value);
    DyAtomicInteger *x = dy_alloc_atomic(&dy_wrapper_atomic_integer, sizeof(DyAtomicInteger));
    atomic_init(&x->value, DY_INT_VALUE(value));
    return DY_OBJ(x);
}

static DyAtomicInteger *as_atomic(D x) {
    if (!dy_kind_p(x, DY_KIND_ATOMIC_INTEGER)) dy_error("not an <atomic-integer>", x);
    return DY_PTR(x);
}

PRIMITIVE(atomic_value) {
    need(self, argc, 1, 1);
    return DY_INT(atomic_load(&as_atomic(a0)->value));
}

/* (atomic-add! a n): add N; answer the new value. */
PRIMITIVE(atomic_add) {
    need(self, argc, 2, 2);
    DyAtomicInteger *x = as_atomic(a0);
    if (!DY_IS_INT(a1)) dy_error("atomic-add!: not an integer", a1);
    intptr_t n = DY_INT_VALUE(a1), old = atomic_load(&x->value), new;
    do {
        new = old + n;                          /* both below 2^62: no wrap */
        if (new < DY_INT_MIN || new > DY_INT_MAX) dy_integer_overflow();
    } while (!atomic_compare_exchange_weak(&x->value, &old, new));
    return DY_INT(new);
}

/* (atomic-compare-and-swap! a old new): if A holds OLD, make it NEW and
 * answer #t; else answer #f. */
PRIMITIVE(atomic_compare_and_swap) {
    need(self, argc, 3, 3);
    DyAtomicInteger *x = as_atomic(a0);
    if (!DY_IS_INT(a1) || !DY_IS_INT(a2)) dy_error("atomic-compare-and-swap!: not an integer", 0);
    intptr_t expected = DY_INT_VALUE(a1);
    return DY_BOOL(atomic_compare_exchange_strong(&x->value, &expected, DY_INT_VALUE(a2)));
}
