/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Tests of the runtime's data representation (Phase 3).  Linked with the
 * data generate.lisp writes: the built-in classes as the compiler emits
 * them, the expected subclass relation, and constants with how each must
 * print.  Run by `make test` in runtime/.
 */

#include "dylan.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

extern const uint8_t test_subclass[];      /* dy_class_count squared */
extern D test_constants[];
extern const char *const test_expected[];
extern const size_t test_constant_count;

static int failures, checks;

#define CHECK(cond) check((cond), #cond, __FILE__, __LINE__)

static void check(bool ok, const char *text, const char *file, int line) {
    checks++;
    if (!ok) {
        failures++;
        printf("FAIL %s:%d: %s\n", file, line, text);
    }
}

/* X as dy_debug_write writes it. */
static char *printed(D x) {
    char *buffer = NULL;
    size_t size = 0;
    FILE *out = open_memstream(&buffer, &size);
    dy_debug_write(out, x);
    fclose(out);
    return buffer;
}

static void check_prints(D x, const char *expected, const char *file, int line) {
    char *got = printed(x);
    checks++;
    if (strcmp(got, expected) != 0) {
        failures++;
        printf("FAIL %s:%d: printed %s, expected %s\n", file, line, got, expected);
    }
    free(got);
}
#define CHECK_PRINTS(x, s) check_prints((x), (s), __FILE__, __LINE__)

/* Run THUNK in a child process; it must stop with a Dylan error whose
 * message contains EXPECTED. */
static void check_error(void (*thunk)(void), const char *expected,
                        const char *file, int line) {
    int pipefd[2];
    if (pipe(pipefd) != 0) { perror("pipe"); exit(2); }
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        close(pipefd[0]);
        dup2(pipefd[1], 2);
        thunk();
        _exit(0);                       /* no error: wrong */
    }
    close(pipefd[1]);
    char message[512];
    ssize_t n = read(pipefd[0], message, sizeof message - 1);
    message[n > 0 ? n : 0] = '\0';
    close(pipefd[0]);
    int status;
    waitpid(pid, &status, 0);
    checks++;
    if (!(WIFEXITED(status) && WEXITSTATUS(status) == 1 && strstr(message, expected))) {
        failures++;
        printf("FAIL %s:%d: expected error \"%s\", got status %d, \"%s\"\n",
               file, line, expected, status, message);
    }
}
#define CHECK_ERROR(thunk, s) check_error((thunk), (s), __FILE__, __LINE__)

/* ----------------------------------------------------------- Immediates */

static void test_immediates(void) {
    CHECK(DY_IS_INT(DY_INT(0)) && DY_INT_VALUE(DY_INT(0)) == 0);
    CHECK(DY_INT_VALUE(DY_INT(-1)) == -1);
    CHECK(DY_INT_VALUE(DY_INT(DY_INT_MAX)) == DY_INT_MAX);
    CHECK(DY_INT_VALUE(DY_INT(DY_INT_MIN)) == DY_INT_MIN);
    CHECK(DY_IS_CHAR(DY_CHAR('a')) && DY_CHAR_CODE(DY_CHAR('a')) == 'a');
    CHECK(DY_CHAR_CODE(DY_CHAR(0x10FFFF)) == 0x10FFFF);
    CHECK(!DY_IS_INT(DY_CHAR('a')) && !DY_IS_POINTER(DY_CHAR('a')));
    CHECK(DY_IS_POINTER(DY_TRUE) && DY_TRUE != DY_FALSE && DY_FALSE != DY_EMPTY);
    CHECK(dy_wrapper_of(DY_INT(5)) == &dy_wrapper_integer);
    CHECK(dy_wrapper_of(DY_CHAR('x')) == &dy_wrapper_character);
    CHECK(dy_wrapper_of(DY_TRUE) == &dy_wrapper_boolean);
    CHECK(dy_wrapper_of(DY_EMPTY) == &dy_wrapper_empty_list);
}

/* -------------------------------------------------------------- Classes */

static void test_classes(void) {
    /* instance? agrees with the precedence lists for every pair, whether
     * it is answered by a range check or by the ancestor bits. */
    int ranges = 0;
    for (size_t i = 0; i < dy_class_count; i++) {
        DyWrapper *w = dy_all_classes[i]->instance_wrapper;
        CHECK(w->id == i);
        CHECK(DY_PTR(w->class) == dy_all_classes[i]);
        if (w->flags & DY_WRAPPER_RANGE_OK) ranges++;
        for (size_t j = 0; j < dy_class_count; j++) {
            DyWrapper *c = dy_all_classes[j]->instance_wrapper;
            bool expected = test_subclass[i * dy_class_count + j];
            if (dy_wrapper_subtype(w, c) != expected) {
                failures++;
                printf("FAIL class %zu subtype of %zu: expected %d\n", i, j, expected);
            }
            checks++;
        }
    }
    CHECK(ranges > 0 && ranges < (int)dy_class_count);   /* both kinds used */
    CHECK(dy_instance_p(DY_INT(3), &dy_wrapper_rational));
    CHECK(dy_instance_p(DY_INT(3), &dy_wrapper_object));
    CHECK(!dy_instance_p(DY_INT(3), &dy_wrapper_float));
    CHECK(dy_instance_p(dy_make_double_float(1.0), &dy_wrapper_real));
    CHECK(dy_instance_p(dy_pair(DY_INT(1), DY_EMPTY), &dy_wrapper_pair));
    CHECK_PRINTS(dy_object_class(DY_INT(1)), "{class <integer>}");
    CHECK_PRINTS(dy_object_class(DY_TRUE), "{class <boolean>}");
}

/* ------------------------------------------------------------ Constants */

static void test_constants_print(void) {
    for (size_t i = 0; i < test_constant_count; i++)
        check_prints(test_constants[i], test_expected[i], __FILE__, (int)i);
    /* 'Foo and 'FOO are one symbol. */
    CHECK(test_constants[17] == test_constants[18]);
}

/* ------------------------------------------------------------- Integers */

static void overflow_add(void) { dy_int_add(DY_INT(DY_INT_MAX), DY_INT(1)); }
static void overflow_sub(void) { dy_int_sub(DY_INT(DY_INT_MIN), DY_INT(1)); }
static void overflow_mul(void) { dy_int_mul(DY_INT(1L << 31), DY_INT(1L << 31)); }
static void overflow_negate(void) { dy_int_negate(DY_INT(DY_INT_MIN)); }
static void overflow_divide(void) { D r; dy_int_truncate(DY_INT(DY_INT_MIN), DY_INT(-1), &r); }
static void divide_zero(void) { D r; dy_int_floor(DY_INT(1), DY_INT(0), &r); }

static void test_integers(void) {
    CHECK(dy_int_add(DY_INT(2), DY_INT(3)) == DY_INT(5));
    CHECK(dy_int_add(DY_INT(-2), DY_INT(-3)) == DY_INT(-5));
    CHECK(dy_int_sub(DY_INT(2), DY_INT(3)) == DY_INT(-1));
    CHECK(dy_int_mul(DY_INT(-6), DY_INT(7)) == DY_INT(-42));
    CHECK(dy_int_mul(DY_INT(1L << 30), DY_INT(1L << 31)) == DY_INT(1L << 61));
    CHECK(dy_int_negate(DY_INT(DY_INT_MAX)) == DY_INT(-DY_INT_MAX));
    CHECK(dy_int_add(DY_INT(DY_INT_MAX), DY_INT(0)) == DY_INT(DY_INT_MAX));
    CHECK(dy_int_sub(DY_INT(DY_INT_MIN), DY_INT(0)) == DY_INT(DY_INT_MIN));
    CHECK(dy_int_less(DY_INT(-1), DY_INT(0)) && !dy_int_less(DY_INT(0), DY_INT(0)));
    CHECK_ERROR(overflow_add, "integer overflow");
    CHECK_ERROR(overflow_sub, "integer overflow");
    CHECK_ERROR(overflow_mul, "integer overflow");
    CHECK_ERROR(overflow_negate, "integer overflow");
    CHECK_ERROR(overflow_divide, "integer overflow");
    CHECK_ERROR(divide_zero, "division by zero");

    /* Chapter 14's divisions: quotient and remainder. */
    static const struct { long x, y, tq, tr, fq, fr, cq, cr, rq, rr; } cases[] = {
        {  7,  2,  3,  1,  3,  1,  4, -1,  4, -1 },
        { -7,  2, -3, -1, -4,  1, -3, -1, -4,  1 },
        {  7, -2, -3,  1, -4, -1, -3,  1, -4, -1 },
        { -7, -2,  3, -1,  3, -1,  4,  1,  4,  1 },
        {  5,  2,  2,  1,  2,  1,  3, -1,  2,  1 },   /* 2.5 rounds to 2 */
        {  6,  4,  1,  2,  1,  2,  2, -2,  2, -2 },   /* 1.5 rounds to 2 */
        {  6,  3,  2,  0,  2,  0,  2,  0,  2,  0 },
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        D x = DY_INT(cases[i].x), y = DY_INT(cases[i].y), r;
        CHECK(dy_int_truncate(x, y, &r) == DY_INT(cases[i].tq) && r == DY_INT(cases[i].tr));
        CHECK(dy_int_floor(x, y, &r) == DY_INT(cases[i].fq) && r == DY_INT(cases[i].fr));
        CHECK(dy_int_ceiling(x, y, &r) == DY_INT(cases[i].cq) && r == DY_INT(cases[i].cr));
        CHECK(dy_int_round(x, y, &r) == DY_INT(cases[i].rq) && r == DY_INT(cases[i].rr));
    }
}

/* -------------------------------------------------------------- Numbers */

static D ratio(long n, long d) { return dy_make_ratio(DY_INT(n), DY_INT(d)); }
static void ratio_overflow(void) { dy_mul(ratio(1, DY_INT_MAX), ratio(1, 3)); }
static void ratio_zero(void) { ratio(1, 0); }
static void not_number(void) { dy_add(DY_INT(1), DY_TRUE); }
static void float_zero(void) { dy_div(dy_make_double_float(1.0), DY_INT(0)); }

static void test_numbers(void) {
    CHECK_PRINTS(ratio(2, 4), "1/2");
    CHECK_PRINTS(ratio(4, -2), "-2");
    CHECK_PRINTS(ratio(0, 5), "0");
    CHECK_PRINTS(dy_add(ratio(1, 3), ratio(2, 3)), "1");
    CHECK_PRINTS(dy_sub(ratio(1, 2), ratio(1, 3)), "1/6");
    CHECK_PRINTS(dy_mul(ratio(2, 3), DY_INT(3)), "2");
    CHECK_PRINTS(dy_div(DY_INT(6), DY_INT(4)), "3/2");
    CHECK_PRINTS(dy_div(DY_INT(6), DY_INT(3)), "2");
    CHECK_PRINTS(dy_negate(ratio(1, 2)), "-1/2");
    /* Exact in 128 bits, then reduced: no spurious overflow. */
    CHECK_PRINTS(dy_mul(ratio(DY_INT_MAX, 3), ratio(3, DY_INT_MAX)), "1");
    CHECK_ERROR(ratio_overflow, "integer overflow");
    CHECK_ERROR(ratio_zero, "division by zero");
    CHECK_ERROR(not_number, "not a number");
    CHECK_ERROR(float_zero, "division by zero");

    /* Float contagion */
    D s = dy_make_single_float(1.5f), d = dy_make_double_float(0.25),
      e = dy_make_extended_float(2.0);
    CHECK_PRINTS(dy_add(s, DY_INT(1)), "2.5s0");
    CHECK_PRINTS(dy_add(DY_INT(1), s), "2.5s0");
    CHECK_PRINTS(dy_add(s, d), "1.75d0");
    CHECK_PRINTS(dy_mul(d, e), "0.5l0");
    CHECK_PRINTS(dy_add(s, e), "3.5l0");
    CHECK_PRINTS(dy_add(ratio(1, 4), d), "0.5d0");
    CHECK_PRINTS(dy_div(DY_INT(1), dy_make_single_float(4.0f)), "0.25s0");
    CHECK_PRINTS(dy_negate(d), "-0.25d0");

    /* Comparison: rationals exactly, floats with rationals exactly. */
    CHECK(dy_compare(ratio(1, 3), ratio(1, 2)) == -1);
    CHECK(dy_compare(DY_INT(2), ratio(3, 2)) == 1);
    CHECK(dy_number_equal(ratio(1, 2), dy_make_double_float(0.5)));
    CHECK(dy_number_equal(dy_make_single_float(0.5f), dy_make_double_float(0.5)));
    CHECK(!dy_number_equal(ratio(1, 10), dy_make_double_float(0.1)));
    CHECK(dy_compare(ratio(1, 10), dy_make_double_float(0.1)) == -1);   /* 0.1d0 > 1/10 */
    CHECK(dy_compare(ratio(1, 3), dy_make_double_float(1.0 / 3.0)) == 1);
    /* 2^62 - 1 is below the double 2^62 it rounds to. */
    CHECK(dy_compare(DY_INT(DY_INT_MAX), dy_make_double_float(0x1p62)) == -1);
    CHECK(dy_compare(DY_INT(9007199254740993L), dy_make_double_float(0x1p53)) == 1);
    CHECK(dy_compare(DY_INT(DY_INT_MIN), dy_make_double_float(-0x1p62)) == 0);
    CHECK(dy_compare(DY_INT(1), dy_make_double_float(1e300)) == -1);
    CHECK(dy_compare(DY_INT(-1), dy_make_double_float(-1e300)) == 1);
    CHECK(dy_compare(ratio(1, DY_INT_MAX), dy_make_double_float(1e-300)) == 1);
    CHECK(dy_compare(DY_INT(0), dy_make_double_float(-1e-300)) == 1);
    CHECK(dy_compare(ratio(1, 1L << 61), dy_make_double_float(0x1p-61)) == 0);
    CHECK(dy_compare(ratio(3, 1L << 61), dy_make_double_float(0x1.8p-60)) == 0);
    CHECK(dy_compare(ratio(3, 1L << 61), dy_make_double_float(0x1.8000000000001p-60)) == -1);
    CHECK(dy_compare(dy_make_double_float(INFINITY), DY_INT(DY_INT_MAX)) == 1);
    CHECK(dy_compare(dy_make_double_float(NAN), DY_INT(1)) == 2);
    CHECK(!dy_number_less(dy_make_double_float(NAN), DY_INT(1)));
    CHECK(!dy_number_equal(dy_make_double_float(NAN), dy_make_double_float(NAN)));
    CHECK(dy_number_less(dy_make_double_float(-0.0), DY_INT(1)));
    CHECK(dy_number_equal(dy_make_double_float(-0.0), DY_INT(0)));
}

/* ---------------------------------------------------- Strings, symbols */

static void test_strings_and_symbols(void) {
    D s = dy_make_c_string("hello");
    CHECK_PRINTS(s, "\"hello\"");
    CHECK(((DyByteString *)DY_PTR(s))->data[5] == '\0');
    uint32_t lambda[] = { 0x3BB, 'x' };
    CHECK_PRINTS(dy_make_unicode_string(lambda, 2), "\"\\U+03BBx\"");

    D a = dy_intern_symbol("Hello", 5), b = dy_intern_symbol("HELLO", 5);
    CHECK(a == b);
    CHECK_PRINTS(b, "Hello");
    CHECK(dy_intern_symbol("hello", 5) != dy_intern_keyword("hello", 5));
    CHECK_PRINTS(dy_intern_keyword("Size", 4), "Size:");
    /* The program's own symbols are interned at startup. */
    CHECK(dy_intern_symbol("<INTEGER>", 9) ==
          ((DyClass *)DY_PTR(dy_wrapper_integer.class))->name);
    CHECK(dy_intern_symbol("foo", 3) == test_constants[17]);
    CHECK(dy_intern_keyword("FOO", 3) == test_constants[16]);
    /* Many symbols: the table grows. */
    char name[32];
    D first = dy_intern_symbol("s0", 2);
    for (int i = 0; i < 5000; i++) {
        int n = snprintf(name, sizeof name, "s%d", i);
        dy_intern_symbol(name, n);
    }
    CHECK(dy_intern_symbol("S0", 2) == first);
}

/* ------------------------------------------------------------------ GC */

static void test_gc(void) {
    /* A long list survives collections while it is reachable. */
    D list = DY_EMPTY;
    for (long i = 0; i < 200000; i++) list = dy_pair(DY_INT(i), list);
    for (int i = 0; i < 20; i++) dy_make_vector(10000, DY_FALSE);   /* garbage */
    GC_gcollect();
    long expect = 199999, ok = 1;
    for (D l = list; l != DY_EMPTY; l = ((DyPair *)DY_PTR(l))->tail, expect--)
        if (((DyPair *)DY_PTR(l))->head != DY_INT(expect)) ok = 0;
    CHECK(ok && expect == -1);
    /* Symbols made at run time are kept by the table. */
    D sym = dy_intern_symbol("kept-by-the-table", 17);
    uintptr_t hidden = ~(uintptr_t)sym;      /* not a pointer the GC sees */
    sym = 0;
    GC_gcollect();
    CHECK(dy_intern_symbol("KEPT-BY-THE-TABLE", 17) == ~hidden);
}

/* ------------------------------------------------------------- Threads */

enum { THREADS = 8, PER_THREAD = 2000 };
static D thread_results[THREADS][PER_THREAD];

static void *thread_body(void *arg) {
    long t = (long)arg;
    char name[32];
    D list = DY_EMPTY;
    for (int i = 0; i < PER_THREAD; i++) {
        /* Every thread interns the same names, in different cases. */
        int n = snprintf(name, sizeof name, t % 2 ? "SHARED-%d" : "shared-%d", i);
        thread_results[t][i] = dy_intern_symbol(name, n);
        list = dy_pair(dy_make_double_float(i), list);
        if (i % 500 == 0) GC_gcollect();
    }
    long count = 0;
    for (D l = list; l != DY_EMPTY; l = ((DyPair *)DY_PTR(l))->tail) count++;
    return (void *)count;
}

static void test_threads(void) {
    pthread_t threads[THREADS];
    for (long t = 0; t < THREADS; t++)
        CHECK(pthread_create(&threads[t], NULL, thread_body, (void *)t) == 0);
    for (long t = 0; t < THREADS; t++) {
        void *count;
        pthread_join(threads[t], &count);
        CHECK((long)count == PER_THREAD);
    }
    long mismatches = 0;
    for (int i = 0; i < PER_THREAD; i++)
        for (int t = 1; t < THREADS; t++)
            if (thread_results[t][i] != thread_results[0][i]) mismatches++;
    CHECK(mismatches == 0);
}

int main(void) {
    dy_init();
    test_immediates();
    test_classes();
    test_constants_print();
    test_integers();
    test_numbers();
    test_strings_and_symbols();
    test_gc();
    test_threads();
    printf("%d checks, %d failures\n", checks, failures);
    return failures != 0;
}
