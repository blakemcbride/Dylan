/*
 * The benchmarks of benchmarks.dyl written by hand in plain C11, for
 * comparison with the Dylan->C build.  Each bench_<name> answers what the
 * Dylan one does, with the same sizes as run-c.lisp.
 *
 *     $ cc -O2 -o reference-c reference.c
 *     $ ./reference-c [name ...]
 *
 * Each benchmark runs three times and prints "NAME VALUE BEST-MS", as the
 * driver run-c.lisp writes for benchmarks.dyl.  Memory comes from malloc
 * and is freed by the benchmark that allocated it.
 */

#define _POSIX_C_SOURCE 199309L
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (!p) { fputs("out of memory\n", stderr); exit(1); }
    return p;
}

static void *xrealloc(void *p, size_t size)
{
    p = realloc(p, size);
    if (!p) { fputs("out of memory\n", stderr); exit(1); }
    return p;
}

/* A linear congruential generator, so all sides see the same numbers. */
static long next_random(long seed)
{
    return (seed * 1103515245L + 12345) % 2147483648L;
}

/* Dylan's RANDOM-LIST conses onto the front, so the last number made
   comes first. */
static long *random_list(long n)
{
    long *v = xmalloc(n * sizeof *v);
    long seed = 42;
    for (long i = 0; i < n; i++) {
        seed = next_random(seed);
        v[n - 1 - i] = seed % 100000;
    }
    return v;
}

/* Hash tables: an open-addressing table with linear probing. */
struct table { long *keys; long *values; bool *used; size_t cap, count; };

static size_t hash_long(long k, size_t cap)
{
    unsigned long h = (unsigned long) k * 0x9E3779B97F4A7C15UL;
    return (size_t) (h >> 32) & (cap - 1);
}

static void table_init(struct table *t, size_t cap)
{
    t->cap = cap;
    t->count = 0;
    t->keys = xmalloc(cap * sizeof *t->keys);
    t->values = xmalloc(cap * sizeof *t->values);
    t->used = calloc(cap, sizeof *t->used);
    if (!t->used) { fputs("out of memory\n", stderr); exit(1); }
}

static void table_free(struct table *t)
{
    free(t->keys); free(t->values); free(t->used);
}

static size_t table_slot(const struct table *t, long key)
{
    size_t i = hash_long(key, t->cap);
    while (t->used[i] && t->keys[i] != key)
        i = (i + 1) & (t->cap - 1);
    return i;
}

static void table_put(struct table *t, long key, long value);

static void table_grow(struct table *t)
{
    struct table old = *t;
    table_init(t, old.cap * 2);
    for (size_t i = 0; i < old.cap; i++)
        if (old.used[i]) table_put(t, old.keys[i], old.values[i]);
    table_free(&old);
}

static void table_put(struct table *t, long key, long value)
{
    if (2 * (t->count + 1) > t->cap) table_grow(t);
    size_t i = table_slot(t, key);
    if (!t->used[i]) { t->used[i] = true; t->keys[i] = key; t->count++; }
    t->values[i] = value;
}

static long table_get(const struct table *t, long key, long dflt)
{
    size_t i = table_slot(t, key);
    return t->used[i] ? t->values[i] : dflt;
}

static long bench_table(long n)
{
    struct table t;
    long sum = 0;
    table_init(&t, 16);
    for (long i = 0; i < n; i++) table_put(&t, i, i * 2);
    for (long i = 0; i < n; i++) table_put(&t, i, table_get(&t, i, 0) + 1);
    for (long i = 0; i < n; i++) sum += table_get(&t, i, 0);
    table_free(&t);
    return sum;
}

/* Sorting with the default test and with a function. */
static int less_than(const void *a, const void *b)
{
    long x = *(const long *) a, y = *(const long *) b;
    return (x > y) - (x < y);
}

static int greater_than(const void *a, const void *b)
{
    return less_than(b, a);
}

static long bench_sort(long n)
{
    long *l = random_list(n);
    long *up = xmalloc(n * sizeof *up);
    long *down = xmalloc(n * sizeof *down);
    memcpy(up, l, n * sizeof *l);
    memcpy(down, l, n * sizeof *l);
    qsort(up, n, sizeof *up, less_than);
    qsort(down, n, sizeof *down, greater_than);
    long result = up[0] + up[n - 1] + down[0] + down[n - 1];
    free(l); free(up); free(down);
    return result;
}

/* Building strings: each piece is its own malloc'd string. */
static char *concatenate3(const char *a, const char *b, const char *c)
{
    size_t la = strlen(a), lb = strlen(b), lc = strlen(c);
    char *s = xmalloc(la + lb + lc + 1);
    memcpy(s, a, la);
    memcpy(s + la, b, lb);
    memcpy(s + la + lb, c, lc + 1);
    return s;
}

static long bench_string(long n)
{
    char **pieces = xmalloc(n * sizeof *pieces);
    long total = 0;
    for (long i = 0; i < n; i++)
        pieces[i] = concatenate3("ab", "cde", i % 2 == 0 ? "x" : "yz");
    for (long i = n - 1; i >= 0; i--) {
        total += (long) strlen(pieces[i]);
        free(pieces[i]);
    }
    free(pieces);
    return total;
}

/* Deques used as queues: a growable ring buffer. */
struct deque { long *data; size_t cap, head, count; };

static void push_last(struct deque *d, long x)
{
    if (d->count == d->cap) {
        size_t cap = d->cap ? d->cap * 2 : 16;
        long *data = xmalloc(cap * sizeof *data);
        for (size_t i = 0; i < d->count; i++)
            data[i] = d->data[(d->head + i) % d->cap];
        free(d->data);
        d->data = data; d->cap = cap; d->head = 0;
    }
    d->data[(d->head + d->count) % d->cap] = x;
    d->count++;
}

static long pop(struct deque *d)
{
    long x = d->data[d->head];
    d->head = (d->head + 1) % d->cap;
    d->count--;
    return x;
}

static long bench_deque(long n)
{
    struct deque d = { NULL, 0, 0, 0 };
    long sum = 0;
    for (long i = 0; i < n; i++) push_last(&d, i);
    for (long i = 0; i < n; i++) sum += pop(&d);
    free(d.data);
    return sum;
}

/* Growing a stretchy vector with realloc. */
static long bench_stretchy(long n)
{
    long *v = NULL;
    size_t size = 0, cap = 0;
    long sum = 0;
    for (long i = 0; i < n; i++) {
        if (size == cap) {
            cap = cap ? cap * 2 : 16;
            v = xrealloc(v, cap * sizeof *v);
        }
        v[size++] = i;
    }
    for (long i = 0; i < n; i++) sum += v[i];
    free(v);
    return sum;
}

/* Iterating over a range. */
static long bench_range(long n)
{
    long sum = 0;
    for (long i = 0; i < n; i++) sum += i;
    return sum;
}

/* Non-local exits: the visitor leaves FIND-FIRST-OVER with longjmp. */
struct list { long first; struct list *rest; };

struct exit_state { jmp_buf exit; long limit, found; };

static void visit_over(struct exit_state *s, long x)
{
    if (x > s->limit) { s->found = x; longjmp(s->exit, 1); }
}

static void for_each(const struct list *l, struct exit_state *s,
                     void (*f)(struct exit_state *, long))
{
    for (; l; l = l->rest) f(s, l->first);
}

static long find_first_over(const struct list *l, long limit)
{
    struct exit_state s;
    s.limit = limit;
    if (setjmp(s.exit)) return s.found;
    for_each(l, &s, visit_over);
    return 0;
}

static long bench_exit(long n)
{
    struct list cells[10];
    long sum = 0;
    for (int i = 0; i < 10; i++) {
        cells[i].first = i + 1;
        cells[i].rest = i < 9 ? &cells[i + 1] : NULL;
    }
    for (long i = 0; i < n; i++) sum += find_first_over(cells, 5);
    return sum;
}

/* Signalling and handling errors: a stack of handlers, each a jmp_buf. */
struct handler { jmp_buf jump; struct handler *next; const char *message; };

static struct handler *handlers = NULL;

static _Noreturn void error(const char *message)
{
    struct handler *h = handlers;
    if (!h) { fprintf(stderr, "unhandled error: %s\n", message); exit(1); }
    handlers = h->next;
    h->message = message;
    longjmp(h->jump, 1);
}

static long maybe_error(long i)
{
    if (i % 2 == 0) error("odd one out");
    return i;
}

/* One HANDLER-CASE: answers whether an error was caught. */
static bool try_maybe_error(long i)
{
    struct handler h;
    h.next = handlers;
    handlers = &h;
    if (setjmp(h.jump)) return true;
    (void) maybe_error(i);
    handlers = h.next;
    return false;
}

static long bench_condition(long n)
{
    long caught = 0;
    for (long i = 0; i < n; i++)
        if (try_maybe_error(i)) caught++;
    return caught;
}

/* A class hierarchy with next-method chains: each class has a superclass
   and a method pointer, and a method calls its superclass's method. */
struct class { const struct class *super; long (*weight)(const struct class *); };

static long weight_animal(const struct class *c) { (void) c; return 1; }
static long weight_mammal(const struct class *c) { return 2 + c->super->weight(c->super); }
static long weight_dog(const struct class *c)    { return 3 + c->super->weight(c->super); }
static long weight_puppy(const struct class *c)  { return 4 + c->super->weight(c->super); }

static const struct class animal_class = { NULL, weight_animal };
static const struct class mammal_class = { &animal_class, weight_mammal };
static const struct class dog_class    = { &mammal_class, weight_dog };
static const struct class puppy_class  = { &dog_class, weight_puppy };

struct object { const struct class *class; };
struct object_list { struct object *first; struct object_list *rest; };

static long weight(const struct object *o) { return o->class->weight(o->class); }

static long bench_next_method(long n)
{
    const struct class *classes[4] = { &animal_class, &mammal_class, &dog_class, &puppy_class };
    struct object_list *animals = NULL;
    long sum = 0;
    for (int i = 3; i >= 0; i--) {
        struct object_list *cell = xmalloc(sizeof *cell);
        cell->first = xmalloc(sizeof *cell->first);
        cell->first->class = classes[i];
        cell->rest = animals;
        animals = cell;
    }
    for (long i = 0; i < n; i++)
        for (struct object_list *a = animals; a; a = a->rest)
            sum += weight(a->first);
    while (animals) {
        struct object_list *rest = animals->rest;
        free(animals->first);
        free(animals);
        animals = rest;
    }
    return sum;
}

/* Recursion with three arguments. */
static long tak(long x, long y, long z)
{
    if (y < x)
        return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
    return z;
}

static long bench_tak(long n)
{
    long sum = 0;
    for (long i = 0; i < n; i++) sum += tak(18, 12, 6);
    return sum;
}

/* Floating-point arithmetic: points of a grid inside the Mandelbrot set. */
static bool mandelbrot_inside(double cr, double ci)
{
    double zr = 0.0, zi = 0.0;
    int i;
    for (i = 0; !(i == 50 || zr * zr + zi * zi > 4.0); i++) {
        double new_zr = zr * zr - zi * zi + cr;
        zi = 2.0 * zr * zi + ci;
        zr = new_zr;
    }
    return i == 50;
}

static long bench_float(long n)
{
    long count = 0;
    for (long y = 0; y < n; y++)
        for (long x = 0; x < n; x++)
            if (mandelbrot_inside(3.0 * x / n - 2.0, 2.0 * y / n - 1.0))
                count++;
    return count;
}

/* The driver. */
static const struct { const char *name; long (*f)(long); long size; } benchmarks[] = {
    { "table", bench_table, 200000 },         { "sort", bench_sort, 100000 },
    { "string", bench_string, 200000 },       { "deque", bench_deque, 300000 },
    { "stretchy", bench_stretchy, 300000 },   { "range", bench_range, 1000000 },
    { "exit", bench_exit, 300000 },           { "condition", bench_condition, 30000 },
    { "next-method", bench_next_method, 300000 }, { "tak", bench_tak, 300 },
    { "float", bench_float, 300 },
};

enum { BENCHMARK_COUNT = sizeof benchmarks / sizeof benchmarks[0] };

static long current_milliseconds(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long) ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

/* BENCH_SCALE, if set, multiplies every size (see run.lisp). */
static long scale(void)
{
    const char *s = getenv("BENCH_SCALE");
    return s && atol(s) > 0 ? atol(s) : 1;
}

static void run(int k)
{
    long best = -1, value = 0;
    for (int i = 0; i < 3; i++) {
        long start = current_milliseconds();
        value = benchmarks[k].f(benchmarks[k].size * scale());
        long elapsed = current_milliseconds() - start;
        if (best < 0 || elapsed < best) best = elapsed;
    }
    printf("%s %ld %ld\n", benchmarks[k].name, value, best);
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        for (int k = 0; k < BENCHMARK_COUNT; k++) run(k);
        return 0;
    }
    for (int a = 1; a < argc; a++) {
        int k = 0;
        while (k < BENCHMARK_COUNT && strcmp(benchmarks[k].name, argv[a]) != 0) k++;
        if (k == BENCHMARK_COUNT) {
            fprintf(stderr, "unknown benchmark: %s\n", argv[a]);
            return 1;
        }
        run(k);
    }
    return 0;
}
