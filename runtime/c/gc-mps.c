/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Memory from the Memory Pool System (runtime/mps), when the runtime is
 * built with DY_MPS, as it is unless a program is built with dylanc --gc
 * boehm: the Boehm-Demers-Weiser collector, which needs nothing here.
 *
 * The MPS moves objects, and so must know every object's layout: this file
 * gives it the object format -- each kind's size and the words of it that
 * are references -- and the roots.  The stacks, and the program's static
 * data, are scanned conservatively: anything there that looks like a
 * pointer into the heap keeps that object alive and in place.  The heap is
 * scanned exactly.  The pools:
 *
 *   AMC    objects that may hold references (generational, moving), each
 *          thread allocating from its own allocation point
 *   AMCZ   objects that hold none: strings, floats, ratios
 *   AMS    <thread>s, which must stay in place (a thread's state points to
 *          its own)
 *   LO     objects holding a mutex or condition variable, which may not
 *          move, nor be protected while the kernel uses them
 *   AWL    the weak table of identity hashes
 *
 * An object's identity hash cannot be its address, since that changes.
 * %object-hash instead gives each object a number the first time it is
 * hashed, kept in a table whose keys are weak (an object's entry dies with
 * it) and that is rebuilt when the MPS reports that a key has moved.
 */

#ifdef DY_MPS

#if !defined(__APPLE__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE                /* pthread_getattr_np */
#endif
#include "dylan.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpscams.h"
#include "mpsclo.h"
#include "mpscawl.h"
#include <stdlib.h>
#include <string.h>
#ifdef __APPLE__
#include <mach-o/getsect.h>
#include <mach-o/ldsyms.h>
#endif

static mps_arena_t arena;
static mps_chain_t chain;
static mps_fmt_t format;
static mps_pool_t amc, amcz, ams, lo, awl;
static mps_ap_t fixed_ap, fixed_leaf_ap, weak_ap;
static pthread_mutex_t shared_lock = PTHREAD_MUTEX_INITIALIZER;

_Thread_local mps_ap_t dy_mps_ap[2];

/* ------------------------------------------------- Objects of our own */

/* The kinds the collector's own objects have, beyond the runtime's */
enum {
    KIND_CELL = 64,                /* DyCell                              */
    KIND_THREAD_SYNC,              /* DyThreadSync                        */
    KIND_WEAK_KEYS,                /* the identity hash table's keys      */
    KIND_FWD2,                     /* forwarded, two words                */
    KIND_FWD,                      /* forwarded: new address and size     */
    KIND_PAD1,                     /* padding, one word                   */
    KIND_PAD                       /* padding, with its size              */
};

#define WRAPPER_OF_KIND(k) { 0, 0, 0, 0, (k), 0, NULL, NULL }
DyWrapper dy_wrapper_cell = WRAPPER_OF_KIND(KIND_CELL);
static DyWrapper wrapper_thread_sync = WRAPPER_OF_KIND(KIND_THREAD_SYNC);
static DyWrapper wrapper_weak_keys = WRAPPER_OF_KIND(KIND_WEAK_KEYS);
static DyWrapper wrapper_fwd2 = WRAPPER_OF_KIND(KIND_FWD2);
static DyWrapper wrapper_fwd = WRAPPER_OF_KIND(KIND_FWD);
static DyWrapper wrapper_pad1 = WRAPPER_OF_KIND(KIND_PAD1);
static DyWrapper wrapper_pad = WRAPPER_OF_KIND(KIND_PAD);

typedef struct {                   /* KIND_FWD2, KIND_FWD                 */
    DyWrapper *wrapper;
    mps_addr_t to;
    size_t size;                   /* KIND_FWD only                       */
} Forward;

typedef struct {                   /* KIND_PAD                            */
    DyWrapper *wrapper;
    size_t size;
} Pad;

typedef struct {                   /* KIND_WEAK_KEYS                      */
    DyWrapper *wrapper;
    D size;                        /* an <integer>                        */
    D keys[];
} WeakKeys;

/* ------------------------------------------------------ The format */

/* The size of the object at BASE, aligned. */
static size_t object_size(mps_addr_t base) {
    DyWrapper *w = ((DyHeader *)base)->wrapper;
    switch (w->kind) {
    case DY_KIND_INSTANCE:
        return DY_ALIGN(sizeof(DyInstance) + w->size * sizeof(D));
    case DY_KIND_PAIR: return DY_ALIGN(sizeof(DyPair));
    case DY_KIND_RATIO: return DY_ALIGN(sizeof(DyRatio));
    case DY_KIND_SINGLE_FLOAT: return DY_ALIGN(sizeof(DySingleFloat));
    case DY_KIND_DOUBLE_FLOAT:
    case DY_KIND_EXTENDED_FLOAT: return DY_ALIGN(sizeof(DyDoubleFloat));
    case DY_KIND_BYTE_STRING:
        return DY_ALIGN(sizeof(DyByteString)
                        + (size_t)DY_INT_VALUE(((DyByteString *)base)->size) + 1);
    case DY_KIND_UNICODE_STRING:
        return DY_ALIGN(sizeof(DyUnicodeString)
                        + (size_t)DY_INT_VALUE(((DyUnicodeString *)base)->size) * 4);
    case DY_KIND_SIMPLE_OBJECT_VECTOR:
        return DY_ALIGN(sizeof(DyVector)
                        + (size_t)DY_INT_VALUE(((DyVector *)base)->size) * sizeof(D));
    case DY_KIND_SYMBOL:
    case DY_KIND_KEYWORD: return DY_ALIGN(sizeof(DySymbol));
    case DY_KIND_FUNCTION:
        return DY_ALIGN(sizeof(DyFunction) + ((DyFunction *)base)->env_count * sizeof(D));
    case DY_KIND_GENERIC: return DY_ALIGN(sizeof(DyGeneric));
    case DY_KIND_SINGLETON: return DY_ALIGN(sizeof(DySingleton));
    case DY_KIND_THREAD: return DY_ALIGN(sizeof(DyThreadObject));
    case DY_KIND_LOCK: return DY_ALIGN(sizeof(DyLock));
    case DY_KIND_CONDITION_VARIABLE: return DY_ALIGN(sizeof(DyConditionVariable));
    case DY_KIND_ATOMIC_INTEGER: return DY_ALIGN(sizeof(DyAtomicInteger));
    case KIND_CELL: return DY_ALIGN(sizeof(DyCell));
    case KIND_THREAD_SYNC: return DY_ALIGN(sizeof(DyThreadSync));
    case KIND_WEAK_KEYS:
        return DY_ALIGN(sizeof(WeakKeys)
                        + (size_t)DY_INT_VALUE(((WeakKeys *)base)->size) * sizeof(D));
    case KIND_FWD2: return DY_ALIGN(2 * sizeof(D));
    case KIND_FWD: return ((Forward *)base)->size;
    case KIND_PAD1: return sizeof(D);
    case KIND_PAD: return ((Pad *)base)->size;
    default:
        fprintf(stderr, "Dylan internal error: an object of kind %u in the heap\n",
                (unsigned)w->kind);
        abort();
    }
}

static mps_addr_t format_skip(mps_addr_t base) {
    return (char *)base + object_size(base);
}

/* Fix the reference at REF, a D: only a pointer can be one. */
#define FIX(ref)                                                        \
    do {                                                                \
        mps_addr_t a_ = (mps_addr_t)(ref);                              \
        if (DY_IS_POINTER((D)a_) && MPS_FIX1(ss, a_)) {                 \
            mps_res_t r_ = MPS_FIX2(ss, &a_);                           \
            if (r_ != MPS_RES_OK) return r_;                            \
            (ref) = (D)a_;                                              \
        }                                                               \
    } while (0)

/* A key that has died, in the identity hash table: not 0, which ends a
 * search. */
static DyHeader dead_key = { &dy_wrapper_object };

static mps_res_t format_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit) {
    MPS_SCAN_BEGIN(ss) {
        while (base < limit) {
            DyWrapper *w = ((DyHeader *)base)->wrapper;
            switch (w->kind) {
            case DY_KIND_INSTANCE: {
                DyInstance *x = base;
                for (uint32_t i = 0; i < w->size; i++) FIX(x->slots[i]);
                break;
            }
            case DY_KIND_PAIR:
                FIX(((DyPair *)base)->head);
                FIX(((DyPair *)base)->tail);
                break;
            case DY_KIND_SIMPLE_OBJECT_VECTOR: {
                DyVector *v = base;
                intptr_t n = DY_INT_VALUE(v->size);
                for (intptr_t i = 0; i < n; i++) FIX(v->data[i]);
                break;
            }
            case DY_KIND_SYMBOL:
            case DY_KIND_KEYWORD:
                FIX(((DySymbol *)base)->name);
                break;
            case DY_KIND_FUNCTION: {
                DyFunction *f = base;
                FIX(f->specializers);
                FIX(f->info);
                FIX(f->owner);
                for (intptr_t i = 0; i < f->env_count; i++) FIX(f->env[i]);
                break;
            }
            case DY_KIND_GENERIC:
                FIX(((DyGeneric *)base)->name);
                FIX(((DyGeneric *)base)->methods);
                FIX(((DyGeneric *)base)->info);
                break;
            case DY_KIND_SINGLETON:
                FIX(((DySingleton *)base)->object);
                break;
            case DY_KIND_THREAD: {
                DyThreadObject *t = base;
                FIX(t->function);
                FIX(t->results);
                FIX(t->condition);
                D sync = (D)t->sync;
                FIX(sync);
                t->sync = (DyThreadSync *)sync;
                break;
            }
            case KIND_CELL:
                FIX(((DyCell *)base)->value);
                break;
            case KIND_WEAK_KEYS: {
                WeakKeys *k = base;
                intptr_t n = DY_INT_VALUE(k->size);
                for (intptr_t i = 0; i < n; i++) {
                    mps_addr_t a = (mps_addr_t)k->keys[i];
                    if (DY_IS_POINTER((D)a) && MPS_FIX1(ss, a)) {
                        mps_res_t r = MPS_FIX2(ss, &a);
                        if (r != MPS_RES_OK) return r;
                        k->keys[i] = a ? (D)a : DY_OBJ(&dead_key);
                    }
                }
                break;
            }
            default:
                break;              /* no references */
            }
            base = (char *)base + object_size(base);
        }
    } MPS_SCAN_END(ss);
    return MPS_RES_OK;
}

static mps_addr_t format_isfwd(mps_addr_t addr) {
    DyWrapper *w = ((DyHeader *)addr)->wrapper;
    return (w == &wrapper_fwd2 || w == &wrapper_fwd) ? ((Forward *)addr)->to : NULL;
}

static void format_fwd(mps_addr_t old, mps_addr_t new) {
    size_t size = object_size(old);
    Forward *f = old;
    if (size == DY_ALIGN(2 * sizeof(D))) {
        f->wrapper = &wrapper_fwd2;
    } else {
        f->wrapper = &wrapper_fwd;
        f->size = size;
    }
    f->to = new;
}

static void format_pad(mps_addr_t addr, size_t size) {
    if (size == sizeof(D)) {
        ((DyHeader *)addr)->wrapper = &wrapper_pad1;
    } else {
        ((Pad *)addr)->wrapper = &wrapper_pad;
        ((Pad *)addr)->size = size;
    }
}

static mps_addr_t no_dependent(mps_addr_t addr) {
    (void)addr;
    return NULL;
}

/* ----------------------------------------------------------- Failure */

_Noreturn void dy_mps_out_of_memory(void) {
    fflush(stdout);
    fputs("Dylan error: out of memory\n", stderr);
    exit(1);
}

static void check(mps_res_t res, const char *what) {
    if (res != MPS_RES_OK) {
        fflush(stdout);
        fprintf(stderr, "Dylan internal error: the memory system could not %s (%d)\n",
                what, (int)res);
        exit(1);
    }
}

/* ------------------------------------------------------------- Roots */

/* The program's static data, which holds the module variables and every
 * object the compiler emits. */
static void register_static_data(void) {
    mps_root_t root;
#ifdef __APPLE__
    const char *segments[] = { "__DATA", "__DATA_DIRTY" };
    for (size_t i = 0; i < sizeof segments / sizeof segments[0]; i++) {
        unsigned long size = 0;
        uint8_t *base = getsegmentdata(&_mh_execute_header, segments[i], &size);
        if (base && size)
            check(mps_root_create_area(&root, arena, mps_rank_ambig(), 0, base,
                                       base + size, mps_scan_area, NULL),
                  "register the static data");
    }
#else
    extern char __data_start[], _end[];
    check(mps_root_create_area(&root, arena, mps_rank_ambig(), 0, __data_start, _end,
                               mps_scan_area, NULL),
          "register the static data");
#endif
}

/* Memory outside the heap that the collector scans, conservatively: a
 * thread's state, the symbol tables.  Its root is just before it. */
typedef struct { mps_root_t root; size_t bytes; } RootHeader;

void *dy_alloc_root(size_t bytes) {
    RootHeader *h = calloc(1, sizeof(RootHeader) + bytes);
    if (!h) dy_mps_out_of_memory();
    h->bytes = bytes;
    char *p = (char *)(h + 1);
    check(mps_root_create_area(&h->root, arena, mps_rank_ambig(), 0, p, p + bytes,
                               mps_scan_area, NULL),
          "register a root");
    return p;
}

void dy_free_root(void *p) {
    if (!p) return;
    RootHeader *h = (RootHeader *)p - 1;
    mps_root_destroy(h->root);
    free(h);
}

/* ----------------------------------------------------------- Threads */

/* The cold end of the calling thread's stack. */
static void *stack_top(void) {
#ifdef __APPLE__
    return pthread_get_stackaddr_np(pthread_self());
#else
    pthread_attr_t attributes;
    void *base;
    size_t size;
    if (pthread_getattr_np(pthread_self(), &attributes) != 0
        || pthread_attr_getstack(&attributes, &base, &size) != 0) {
        fputs("Dylan internal error: cannot find the thread's stack\n", stderr);
        exit(1);
    }
    pthread_attr_destroy(&attributes);
    return (char *)base + size;
#endif
}

/* Register the calling thread, whose state is dy_thread: its stack is a
 * root, and it gets allocation points of its own. */
void dy_mps_thread_start(void) {
    DyThread *t = dy_thread;
    check(mps_thread_reg(&t->mps_thread, arena), "register a thread");
    check(mps_root_create_thread(&t->stack_root, arena, t->mps_thread, stack_top()),
          "register a thread's stack");
    check(mps_ap_create_k(&dy_mps_ap[DY_POOL_EXACT], amc, mps_args_none),
          "make an allocation point");
    check(mps_ap_create_k(&dy_mps_ap[DY_POOL_LEAF], amcz, mps_args_none),
          "make an allocation point");
}

void dy_mps_thread_end(void) {
    DyThread *t = dy_thread;
    mps_ap_destroy(dy_mps_ap[DY_POOL_EXACT]);
    mps_ap_destroy(dy_mps_ap[DY_POOL_LEAF]);
    mps_root_destroy(t->stack_root);
    mps_thread_dereg(t->mps_thread);
}

/* ------------------------------------------------ Collecting the world */

/* The MPS collects a generation when it fills, but the oldest -- where
 * what survives the chain ends up -- only when the arena is nearly out of
 * the address space it has reserved, which, with no commit limit, is
 * seldom: garbage promoted there would stay, and the program's memory
 * grow to several times what it uses.  So, as the Boehm collector does,
 * a collection of everything starts whenever the memory in use has
 * doubled since the last one left it.  Every refill of an allocation
 * point -- about every megabyte allocated -- checks. */

#define FULL_COLLECTION_GROWTH ((size_t)64 << 20)

static _Atomic size_t in_use_floor;    /* the least in use since the last one started */
static size_t nursery_bytes = 128 * 1024 * 1024;   /* set by dy_gc_init */

mps_res_t dy_mps_fill(mps_addr_t *p, mps_ap_t ap, size_t bytes) {
    size_t in_use = mps_arena_committed(arena) - mps_arena_spare_committed(arena);
    size_t floor = atomic_load_explicit(&in_use_floor, memory_order_relaxed);
    /* The growth since must be at least what was in use then (so that the
     * heap at most doubles), and at least the nursery and 64 MB besides
     * (so that a nursery filling is not taken for growth). */
    size_t growth = floor > nursery_bytes + FULL_COLLECTION_GROWTH
                    ? floor : nursery_bytes + FULL_COLLECTION_GROWTH;
    if (in_use < floor) {
        atomic_store_explicit(&in_use_floor, in_use, memory_order_relaxed);
    } else if (in_use - floor > growth && mps_arena_start_collect(arena) == MPS_RES_OK) {
        /* From here the floor can only come down, as the collection frees. */
        atomic_store_explicit(&in_use_floor, in_use, memory_order_relaxed);
    }
    return mps_ap_fill(p, ap, bytes);
}

/* -------------------------------------------------- Allocation, shared */

static void *reserve_commit(mps_ap_t ap, DyWrapper *wrapper, size_t bytes, D size,
                            bool clear) {
    bytes = DY_ALIGN(bytes);
    mps_addr_t p;
    do {
        if (!dy_mps_reserve(&p, ap, bytes)) dy_mps_out_of_memory();
        if (clear) memset(p, 0, bytes);
        ((DyHeader *)p)->wrapper = wrapper;
        if (size) ((D *)p)[1] = size;
    } while (!dy_mps_commit(ap, p, bytes));
    return p;
}

void *dy_mps_alloc_shared(int pool, DyWrapper *wrapper, size_t bytes) {
    pthread_mutex_lock(&shared_lock);
    void *p = reserve_commit(pool == DY_POOL_FIXED ? fixed_ap : fixed_leaf_ap,
                             wrapper, bytes, 0, true);
    pthread_mutex_unlock(&shared_lock);
    return p;
}

/* A <thread>'s mutex and condition variable */
struct DyThreadSync *dy_mps_make_thread_sync(void) {
    return dy_mps_alloc_shared(DY_POOL_FIXED_LEAF, &wrapper_thread_sync, sizeof(DyThreadSync));
}

/* --------------------------------------------------------- Collection */

void dy_gc_collect(void) {
    mps_arena_collect(arena);
    mps_arena_release(arena);
}

static long stress_interval = 100;
static long stress_count;

void dy_mps_stress(void) {
    if (__atomic_add_fetch(&stress_count, 1, __ATOMIC_RELAXED) % stress_interval == 0)
        dy_gc_collect();
}

/* ------------------------------------------------------ Identity hashes */

static pthread_mutex_t hash_lock = PTHREAD_MUTEX_INITIALIZER;
static WeakKeys *hash_keys;        /* in AWL: weak                        */
static intptr_t *hash_ids;         /* the number of each key              */
static size_t hash_capacity, hash_used;  /* used counts dead keys too    */
static mps_ld_s hash_ld;           /* the keys' addresses it depends on   */
static intptr_t hash_next = 1;

static size_t address_slot(D x, size_t capacity) {
    uint64_t h = (uint64_t)x * 0x9E3779B97F4A7C15u;
    return (size_t)(h >> 20) & (capacity - 1);
}

/* The keys and ids as they are, placed again by the keys' addresses now,
 * in a table of CAPACITY, dead keys dropped.  HASH_LOCK is held. */
static void rebuild(size_t capacity) {
    WeakKeys *old = hash_keys;
    intptr_t *old_ids = hash_ids;
    size_t old_capacity = hash_capacity;
    pthread_mutex_lock(&shared_lock);
    WeakKeys *keys = reserve_commit(weak_ap, &wrapper_weak_keys,
                                    sizeof(WeakKeys) + capacity * sizeof(D),
                                    DY_INT(capacity), true);
    pthread_mutex_unlock(&shared_lock);
    intptr_t *ids = calloc(capacity, sizeof(intptr_t));
    if (!ids) dy_mps_out_of_memory();
    mps_ld_reset(&hash_ld, arena);
    size_t used = 0;
    for (size_t i = 0; i < old_capacity; i++) {
        D key = old->keys[i];
        if (key == 0 || key == DY_OBJ(&dead_key)) continue;
        mps_ld_add(&hash_ld, arena, (mps_addr_t)key);
        size_t j = address_slot(key, capacity);
        while (keys->keys[j]) j = (j + 1) & (capacity - 1);
        keys->keys[j] = key;
        ids[j] = old_ids[i];
        used++;
    }
    hash_keys = keys;
    hash_ids = ids;
    hash_capacity = capacity;
    hash_used = used;
    free(old_ids);
}

/* X's number, or 0 if it has none.  HASH_LOCK is held. */
static intptr_t find_id(D x) {
    if (!hash_capacity) return 0;
    for (size_t i = address_slot(x, hash_capacity); hash_keys->keys[i];
         i = (i + 1) & (hash_capacity - 1))
        if (hash_keys->keys[i] == x) return hash_ids[i];
    return 0;
}

/* A number for X, which is in the heap: the same each time. */
static intptr_t identity_number(D x) {
    pthread_mutex_lock(&hash_lock);
    intptr_t id = find_id(x);
    if (!id && hash_capacity && mps_ld_isstale(&hash_ld, arena, (mps_addr_t)x)) {
        /* Keys have moved: place them again. */
        rebuild(hash_capacity);
        id = find_id(x);
    }
    if (!id) {
        if ((hash_used + 1) * 2 > hash_capacity)
            rebuild(hash_capacity ? hash_capacity * 2 : 1024);
        id = hash_next++;
        mps_ld_add(&hash_ld, arena, (mps_addr_t)x);
        size_t i = address_slot(x, hash_capacity);
        while (hash_keys->keys[i]) i = (i + 1) & (hash_capacity - 1);
        hash_keys->keys[i] = x;
        hash_ids[i] = id;
        hash_used++;
    }
    pthread_mutex_unlock(&hash_lock);
    return id;
}

/* (%object-hash x): an identity hash */
D dy_object_hash(D x) {
    uint64_t h = DY_IS_POINTER(x) && mps_arena_has_addr(arena, (mps_addr_t)x)
                     ? (uint64_t)identity_number(x)
                     : (uint64_t)x;
    h *= 0x9E3779B97F4A7C15u;
    return DY_INT((intptr_t)(h >> 4) & DY_INT_MAX);
}

/* ------------------------------------------------------------- Start */

/* How much memory a moving pool takes at once: a little at a time, the
 * default, costs a system call for each.  DYLAN_MPS_EXTEND (bytes) says
 * otherwise. */
#define EXTEND_BY (1024 * 1024)

/* The generations: each one's capacity in kilobytes, and the fraction of
 * its objects expected to survive a collection of it.  The nursery is
 * large: a program that builds a large table, vector or list and keeps
 * writing into it pays, at each collection of the nursery, for
 * protecting the old objects again and for faults on the pages it then
 * writes, and a nursery of 32 MB made such programs slower than under
 * the Boehm collector, where 128 MB makes them faster; the price is the
 * memory, about twice the Boehm collector's for a program that allocates
 * heavily.  DYLAN_MPS_NURSERY (kilobytes) says otherwise. */
static mps_gen_param_s generations[] = {
    { 128 * 1024, 0.85 },
    { 128 * 1024, 0.45 }
};

void dy_gc_init(void) {
    check(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
          "make its arena");
    /* Memory freed is kept, not given back to the system: given back, it
     * must be faulted in afresh when the nursery next fills, which can
     * take longer than the collecting.  DYLAN_MPS_SPARE (the fraction of
     * the memory it holds that the MPS may keep unused) says otherwise. */
    const char *extend = getenv("DYLAN_MPS_EXTEND");
    size_t extend_by = extend ? (size_t)atol(extend) : EXTEND_BY;
    const char *spare = getenv("DYLAN_MPS_SPARE");
    mps_arena_spare_set(arena, spare ? atof(spare) : 1.0);
    const char *pause = getenv("DYLAN_MPS_PAUSE");
    if (pause) mps_arena_pause_time_set(arena, atof(pause));
    const char *nursery = getenv("DYLAN_MPS_NURSERY");
    if (nursery && atol(nursery) > 0) generations[0].mps_capacity = (size_t)atol(nursery);
    nursery_bytes = generations[0].mps_capacity * 1024;
    check(mps_chain_create(&chain, arena, sizeof generations / sizeof generations[0],
                           generations),
          "make its generations");
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, sizeof(D));
        MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, format_scan);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, format_skip);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, format_fwd);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, format_isfwd);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, format_pad);
        check(mps_fmt_create_k(&format, arena, args), "make its object format");
    } MPS_ARGS_END(args);
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
        MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
        if (extend_by) {
            MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, extend_by);
            MPS_ARGS_ADD(args, MPS_KEY_LARGE_SIZE, extend_by);   /* at least EXTEND_BY */
        }
        check(mps_pool_create_k(&amc, arena, mps_class_amc(), args), "make a pool");
    } MPS_ARGS_END(args);
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
        MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
        if (extend_by) {
            MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, extend_by);
            MPS_ARGS_ADD(args, MPS_KEY_LARGE_SIZE, extend_by);   /* at least EXTEND_BY */
        }
        check(mps_pool_create_k(&amcz, arena, mps_class_amcz(), args), "make a pool");
    } MPS_ARGS_END(args);
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
        check(mps_pool_create_k(&ams, arena, mps_class_ams(), args), "make a pool");
    } MPS_ARGS_END(args);
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
        check(mps_pool_create_k(&lo, arena, mps_class_lo(), args), "make a pool");
    } MPS_ARGS_END(args);
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
        MPS_ARGS_ADD(args, MPS_KEY_AWL_FIND_DEPENDENT, no_dependent);
        check(mps_pool_create_k(&awl, arena, mps_class_awl(), args), "make a pool");
    } MPS_ARGS_END(args);
    check(mps_ap_create_k(&fixed_ap, ams, mps_args_none), "make an allocation point");
    check(mps_ap_create_k(&fixed_leaf_ap, lo, mps_args_none), "make an allocation point");
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_weak());
        check(mps_ap_create_k(&weak_ap, awl, args), "make an allocation point");
    } MPS_ARGS_END(args);
    register_static_data();
#ifdef DY_GC_STRESS
    const char *interval = getenv("DYLAN_GC_STRESS");
    if (interval && atol(interval) > 0) stress_interval = atol(interval);
#endif
}

#endif
