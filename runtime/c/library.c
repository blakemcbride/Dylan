/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * The C primitives the library (the files of runtime/dylan) is written on
 * (CompilerPlan.md, Phase 8).  Their names begin with %; they are the
 * DYLAN module's own, not exported.  Each does one small thing for the
 * built-in representations -- arithmetic, characters, strings, vectors,
 * symbols, hashing, output -- and checks its arguments only as far as it
 * must to stay safe.
 */

#include "primitive.h"
#include <math.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <wctype.h>

/* ------------------------------------------------------------- Numbers */

PRIMITIVE(number_add) { need(self, argc, 2, 2); return dy_add(a0, a1); }
PRIMITIVE(number_sub) { need(self, argc, 2, 2); return dy_sub(a0, a1); }
PRIMITIVE(number_mul) { need(self, argc, 2, 2); return dy_mul(a0, a1); }
PRIMITIVE(number_div) { need(self, argc, 2, 2); return dy_div(a0, a1); }
PRIMITIVE(number_negate) { need(self, argc, 1, 1); return dy_negate(a0); }
PRIMITIVE(number_equal) { need(self, argc, 2, 2); return DY_BOOL(dy_number_equal(a0, a1)); }
PRIMITIVE(number_less) { need(self, argc, 2, 2); return DY_BOOL(dy_number_less(a0, a1)); }
PRIMITIVE(number_p) { need(self, argc, 1, 1); return DY_BOOL(dy_number_p(a0)); }

static intptr_t int_arg(D x) {
    if (!DY_IS_INT(x)) dy_error("not an integer", x);
    return DY_INT_VALUE(x);
}

static bool float_p(D x) {
    return dy_kind_p(x, DY_KIND_SINGLE_FLOAT) || dy_kind_p(x, DY_KIND_DOUBLE_FLOAT)
        || dy_kind_p(x, DY_KIND_EXTENDED_FLOAT);
}

/* A float of the format of X (or a double-float if X is rational). */
static D float_like(D x, double value) {
    if (dy_kind_p(x, DY_KIND_SINGLE_FLOAT)) return dy_make_single_float((float)value);
    if (dy_kind_p(x, DY_KIND_EXTENDED_FLOAT)) return dy_make_extended_float(value);
    return dy_make_double_float(value);
}

static D float_like2(D x, D y, double value) {
    if (dy_kind_p(x, DY_KIND_EXTENDED_FLOAT) || dy_kind_p(y, DY_KIND_EXTENDED_FLOAT))
        return dy_make_extended_float(value);
    if (dy_kind_p(x, DY_KIND_DOUBLE_FLOAT) || dy_kind_p(y, DY_KIND_DOUBLE_FLOAT))
        return dy_make_double_float(value);
    if (dy_kind_p(x, DY_KIND_SINGLE_FLOAT) || dy_kind_p(y, DY_KIND_SINGLE_FLOAT))
        return dy_make_single_float((float)value);
    return dy_make_double_float(value);
}

static D two_values(D first, D second) {
    DyThread *t = dy_thread;
    t->mv[0] = first;
    t->mv[1] = second;
    t->mv_count = 2;
    return first;
}

static D integer_of_double(double q) {
    if (!(q >= -0x1p62 && q < 0x1p62)) dy_integer_overflow();
    return DY_INT((intptr_t)q);
}

enum { FLOOR, CEILING, ROUND, TRUNCATE };

/* (%divide kind a b): chapter 14's floor/, ceiling/, round/ and truncate/:
 * an integer quotient and the remainder a - q * b. */
PRIMITIVE(divide) {
    need(self, argc, 3, 3);
    int kind = (int)int_arg(a0);
    D a = a1, b = a2, r;
    if (DY_IS_INT(a) && DY_IS_INT(b)) {
        D q;
        switch (kind) {
        case FLOOR: q = dy_int_floor(a, b, &r); break;
        case CEILING: q = dy_int_ceiling(a, b, &r); break;
        case ROUND: q = dy_int_round(a, b, &r); break;
        default: q = dy_int_truncate(a, b, &r); break;
        }
        return two_values(q, r);
    }
    if (!dy_number_p(a)) dy_error("not a real number", a);
    if (!dy_number_p(b)) dy_error("not a real number", b);
    if (dy_number_equal(b, DY_INT(0))) dy_division_by_zero();
    if (!float_p(a) && !float_p(b)) {
        /* Rationals: the quotient a / b exactly, then rounded. */
        D quotient = dy_div(a, b);
        D n = quotient, d = DY_INT(1);
        if (dy_kind_p(quotient, DY_KIND_RATIO)) {
            n = ((DyRatio *)DY_PTR(quotient))->numerator;
            d = ((DyRatio *)DY_PTR(quotient))->denominator;
        }
        D q;
        switch (kind) {
        case FLOOR: q = dy_int_floor(n, d, &r); break;
        case CEILING: q = dy_int_ceiling(n, d, &r); break;
        case ROUND: q = dy_int_round(n, d, &r); break;
        default: q = dy_int_truncate(n, d, &r); break;
        }
        return two_values(q, dy_sub(a, dy_mul(q, b)));
    }
    double x = dy_float_value(a), y = dy_float_value(b), q = x / y;
    switch (kind) {
    case FLOOR: q = floor(q); break;
    case CEILING: q = ceil(q); break;
    case ROUND: q = nearbyint(q); break;      /* ties to even */
    default: q = trunc(q); break;
    }
    return two_values(integer_of_double(q), float_like2(a, b, x - q * y));
}

/* (%math op x [y]): the irrational functions, on floats.  A rational
 * argument gives a <double-float> (the manual's examples). */
PRIMITIVE(math) {
    need(self, argc, 2, 3);
    if (!dy_number_p(a1)) dy_error("not a number", a1);
    double x = dy_float_value(a1), r;
    switch (int_arg(a0)) {
    case 0: r = sin(x); break;
    case 1: r = cos(x); break;
    case 2: r = tan(x); break;
    case 3: if (x < -1 || x > 1) dy_error("asin: the result would be complex", a1);
            r = asin(x); break;
    case 4: if (x < -1 || x > 1) dy_error("acos: the result would be complex", a1);
            r = acos(x); break;
    case 5: r = atan(x); break;
    case 6: r = sinh(x); break;
    case 7: r = cosh(x); break;
    case 8: r = tanh(x); break;
    case 9: r = asinh(x); break;
    case 10: if (x < 1) dy_error("acosh: the result would be complex", a1);
             r = acosh(x); break;
    case 11: if (x <= -1 || x >= 1) dy_error("atanh: the argument is out of range", a1);
             r = atanh(x); break;
    case 12: r = exp(x); break;
    case 13: if (x <= 0) dy_error("log: the argument must be positive", a1);
             r = log(x); break;
    case 14: if (x < 0) dy_error("sqrt: the result would be complex", a1);
             r = sqrt(x); break;
    case 15: {
        if (argc != 3 || !dy_number_p(a2)) dy_error("atan2: not a number", a2);
        double y = dy_float_value(a2);
        return float_like2(a1, a2, atan2(x, y));
    }
    case 16: {                                 /* expt with a float */
        if (argc != 3 || !dy_number_p(a2)) dy_error("expt: not a number", a2);
        double y = dy_float_value(a2);
        if (x < 0 && y != floor(y)) dy_error("expt: the result would be complex", a1);
        return float_like2(a1, a2, pow(x, y));
    }
    default: dy_error("%math: unknown operation", a0);
    }
    return float_like(a1, r);
}

PRIMITIVE(float_p) { need(self, argc, 1, 1); return DY_BOOL(float_p(a0)); }

/* (%float x format): X as a single (FORMAT 0), double (1) or extended (2)
 * float. */
PRIMITIVE(to_float) {
    need(self, argc, 2, 2);
    if (!dy_number_p(a0)) dy_error("not a number", a0);
    double x = dy_float_value(a0);
    switch (int_arg(a1)) {
    case 0: return dy_make_single_float((float)x);
    case 2: return dy_make_extended_float(x);
    default: return dy_make_double_float(x);
    }
}

PRIMITIVE(numerator) {
    need(self, argc, 1, 1);
    if (DY_IS_INT(a0)) return a0;
    if (dy_kind_p(a0, DY_KIND_RATIO)) return ((DyRatio *)DY_PTR(a0))->numerator;
    dy_error("not a rational", a0);
}

PRIMITIVE(denominator) {
    need(self, argc, 1, 1);
    if (DY_IS_INT(a0)) return DY_INT(1);
    if (dy_kind_p(a0, DY_KIND_RATIO)) return ((DyRatio *)DY_PTR(a0))->denominator;
    dy_error("not a rational", a0);
}

/* (%rationalize x): the simplest rational within X's precision, by
 * continued fractions, numerator and denominator kept within 63 bits. */
PRIMITIVE(rationalize) {
    need(self, argc, 1, 1);
    if (!float_p(a0)) return a0;
    double x = dy_float_value(a0);
    if (!isfinite(x)) dy_error("rationalize: not a finite number", a0);
    double epsilon = dy_kind_p(a0, DY_KIND_SINGLE_FLOAT) ? fabs(x) * 6e-8 : fabs(x) * 1.2e-16;
    long double h0 = 0, h1 = 1, k0 = 1, k1 = 0, v = x;
    for (int i = 0; i < 64; i++) {
        long double a = floorl(v);
        long double h2 = a * h1 + h0, k2 = a * k1 + k0;
        if (fabsl(h2) > 0x1p62 || k2 > 0x1p62) break;
        h0 = h1; h1 = h2; k0 = k1; k1 = k2;
        if (fabsl((long double)x - h1 / k1) <= epsilon) break;
        if (v == a) break;
        v = 1 / (v - a);
    }
    return dy_make_ratio(DY_INT((intptr_t)h1), DY_INT((intptr_t)k1));
}

/* Integers as bits (two's complement, as Common Lisp) */
PRIMITIVE(logand) { need(self, argc, 2, 2); return DY_INT(int_arg(a0) & int_arg(a1)); }
PRIMITIVE(logior) { need(self, argc, 2, 2); return DY_INT(int_arg(a0) | int_arg(a1)); }
PRIMITIVE(logxor) { need(self, argc, 2, 2); return DY_INT(int_arg(a0) ^ int_arg(a1)); }
PRIMITIVE(lognot) { need(self, argc, 1, 1); return DY_INT(~int_arg(a0)); }

PRIMITIVE(logbit_p) {
    need(self, argc, 2, 2);
    intptr_t index = int_arg(a0), n = int_arg(a1);
    if (index < 0) dy_error("logbit?: negative index", a0);
    return DY_BOOL(index >= 63 ? n < 0 : (n >> index) & 1);
}

PRIMITIVE(ash) {
    need(self, argc, 2, 2);
    intptr_t n = int_arg(a0), count = int_arg(a1);
    if (count <= 0) return DY_INT(count <= -63 ? (n < 0 ? -1 : 0) : n >> -count);
    if (n == 0) return DY_INT(0);
    if (count >= 63) dy_integer_overflow();
    intptr_t r = (intptr_t)((uintptr_t)n << count);
    if ((r >> count) != n) dy_integer_overflow();
    return dy_make_int(r);
}

PRIMITIVE(gcd) {
    need(self, argc, 2, 2);
    intptr_t x = int_arg(a0), y = int_arg(a1);
    if (x < 0) x = -x;
    if (y < 0) y = -y;
    while (y) { intptr_t t = x % y; x = y; y = t; }
    return dy_make_int(x);
}

/* ---------------------------------------------------------- Characters */

PRIMITIVE(character_to_integer) {
    need(self, argc, 1, 1);
    if (!DY_IS_CHAR(a0)) dy_error("not a character", a0);
    return DY_INT(DY_CHAR_CODE(a0));
}

PRIMITIVE(integer_to_character) {
    need(self, argc, 1, 1);
    intptr_t c = int_arg(a0);
    if (c < 0 || c > 0x10FFFF) dy_error("not a character code", a0);
    return DY_CHAR(c);
}

PRIMITIVE(character_p) { need(self, argc, 1, 1); return DY_BOOL(DY_IS_CHAR(a0)); }

PRIMITIVE(char_upcase) {
    need(self, argc, 1, 1);
    if (!DY_IS_CHAR(a0)) dy_error("not a character", a0);
    return DY_CHAR(towupper(DY_CHAR_CODE(a0)));
}

PRIMITIVE(char_downcase) {
    need(self, argc, 1, 1);
    if (!DY_IS_CHAR(a0)) dy_error("not a character", a0);
    return DY_CHAR(towlower(DY_CHAR_CODE(a0)));
}

/* ------------------------------------------------- Vectors and strings
 *
 * One set for the three built-in vectors: <simple-object-vector>,
 * <byte-string> and <unicode-string>. */

static intptr_t index_arg(D v, D i, intptr_t size) {
    if (!DY_IS_INT(i)) dy_error("element: the index is not an integer", i);
    intptr_t k = DY_INT_VALUE(i);
    if (k < 0 || k >= size) dy_error("element: the index is out of range", i);
    (void)v;
    return k;
}

static intptr_t vector_size(D v) {
    if (dy_kind_p(v, DY_KIND_SIMPLE_OBJECT_VECTOR)) return DY_INT_VALUE(((DyVector *)DY_PTR(v))->size);
    if (dy_kind_p(v, DY_KIND_BYTE_STRING)) return DY_INT_VALUE(((DyByteString *)DY_PTR(v))->size);
    if (dy_kind_p(v, DY_KIND_UNICODE_STRING)) return DY_INT_VALUE(((DyUnicodeString *)DY_PTR(v))->size);
    dy_error("not a vector", v);
}

PRIMITIVE(vector_size) { need(self, argc, 1, 1); return DY_INT(vector_size(a0)); }

PRIMITIVE(vector_element) {
    need(self, argc, 2, 2);
    intptr_t k = index_arg(a0, a1, vector_size(a0));
    if (dy_kind_p(a0, DY_KIND_SIMPLE_OBJECT_VECTOR)) return ((DyVector *)DY_PTR(a0))->data[k];
    if (dy_kind_p(a0, DY_KIND_BYTE_STRING))
        return DY_CHAR((unsigned char)((DyByteString *)DY_PTR(a0))->data[k]);
    return DY_CHAR(((DyUnicodeString *)DY_PTR(a0))->data[k]);
}

PRIMITIVE(vector_element_setter) {
    need(self, argc, 3, 3);
    intptr_t k = index_arg(a0, a1, vector_size(a0));
    if (dy_kind_p(a0, DY_KIND_SIMPLE_OBJECT_VECTOR)) {
        ((DyVector *)DY_PTR(a0))->data[k] = a2;
    } else if (dy_kind_p(a0, DY_KIND_BYTE_STRING)) {
        if (!DY_IS_CHAR(a2) || DY_CHAR_CODE(a2) > 255)
            dy_error("a <byte-string> holds only 8-bit characters", a2);
        ((DyByteString *)DY_PTR(a0))->data[k] = (char)DY_CHAR_CODE(a2);
    } else {
        if (!DY_IS_CHAR(a2)) dy_error("a string holds only characters", a2);
        ((DyUnicodeString *)DY_PTR(a0))->data[k] = DY_CHAR_CODE(a2);
    }
    return a2;
}

PRIMITIVE(make_vector) {
    need(self, argc, 2, 2);
    intptr_t n = int_arg(a0);
    if (n < 0) dy_error("make: negative size", a0);
    return dy_make_vector(n, a1);
}

PRIMITIVE(make_byte_string) {
    need(self, argc, 2, 2);
    intptr_t n = int_arg(a0);
    if (n < 0) dy_error("make: negative size", a0);
    if (!DY_IS_CHAR(a1) || DY_CHAR_CODE(a1) > 255)
        dy_error("make: a <byte-string> holds only 8-bit characters", a1);
    char *bytes = GC_MALLOC_ATOMIC(n + 1);
    memset(bytes, (int)DY_CHAR_CODE(a1), n);
    return dy_make_byte_string(bytes, n);
}

PRIMITIVE(make_unicode_string) {
    need(self, argc, 2, 2);
    intptr_t n = int_arg(a0);
    if (n < 0) dy_error("make: negative size", a0);
    if (!DY_IS_CHAR(a1)) dy_error("make: fill: must be a character", a1);
    uint32_t *chars = GC_MALLOC_ATOMIC((n + 1) * 4);
    for (intptr_t i = 0; i < n; i++) chars[i] = DY_CHAR_CODE(a1);
    return dy_make_unicode_string(chars, n);
}

/* (%replace-vector! target target-start source source-start count) */
PRIMITIVE(replace_vector) {
    need(self, argc, 5, 5);
    intptr_t ts = int_arg(a1), ss = int_arg(a3), n = int_arg(ARG(4));
    if (n < 0 || ts < 0 || ss < 0 || ts + n > vector_size(a0) || ss + n > vector_size(a2))
        dy_error("replace: out of range", 0);
    if (dy_kind_p(a0, DY_KIND_SIMPLE_OBJECT_VECTOR) && dy_kind_p(a2, DY_KIND_SIMPLE_OBJECT_VECTOR)) {
        memmove(&((DyVector *)DY_PTR(a0))->data[ts], &((DyVector *)DY_PTR(a2))->data[ss],
                n * sizeof(D));
        return a0;
    }
    if (dy_kind_p(a0, DY_KIND_BYTE_STRING) && dy_kind_p(a2, DY_KIND_BYTE_STRING)) {
        memmove(&((DyByteString *)DY_PTR(a0))->data[ts], &((DyByteString *)DY_PTR(a2))->data[ss], n);
        return a0;
    }
    /* Mixed: element by element */
    D in_registers[4] = { a0, a1, a2, a3 };
    (void)in_registers;
    if (ts <= ss) {
        for (intptr_t i = 0; i < n; i++) {
            D x = dy_prim_vector_element_entry(self, a2, DY_INT(ss + i), 0, 0, 2);
            dy_prim_vector_element_setter_entry(self, a0, DY_INT(ts + i), x, 0, 3);
        }
    } else {
        for (intptr_t i = n - 1; i >= 0; i--) {
            D x = dy_prim_vector_element_entry(self, a2, DY_INT(ss + i), 0, 0, 2);
            dy_prim_vector_element_setter_entry(self, a0, DY_INT(ts + i), x, 0, 3);
        }
    }
    return a0;
}

/* (%concatenate-byte-strings string strings): a new byte string of STRING
 * and then the elements of the list STRINGS, or #f if one is not a byte
 * string. */
PRIMITIVE(concatenate_byte_strings) {
    need(self, argc, 2, 2);
    if (!dy_kind_p(a0, DY_KIND_BYTE_STRING)) return DY_FALSE;
    size_t n = DY_STRING_SIZE(a0);
    for (D l = a1; l != DY_EMPTY; l = dy_tail(l)) {
        if (!dy_kind_p(l, DY_KIND_PAIR) || !dy_kind_p(dy_head(l), DY_KIND_BYTE_STRING))
            return DY_FALSE;
        n += DY_STRING_SIZE(dy_head(l));
    }
    if (n > (size_t)DY_INT_MAX) dy_error("string too large", 0);
    DyByteString *s = dy_alloc_atomic(&dy_wrapper_byte_string, sizeof(DyByteString) + n + 1);
    s->size = DY_INT(n);
    size_t at = DY_STRING_SIZE(a0);
    memcpy(s->data, DY_STRING_DATA(a0), at);
    for (D l = a1; l != DY_EMPTY; l = dy_tail(l)) {
        size_t k = DY_STRING_SIZE(dy_head(l));
        memcpy(s->data + at, DY_STRING_DATA(dy_head(l)), k);
        at += k;
    }
    s->data[n] = '\0';
    return DY_OBJ(s);
}

/* --------------------------------------------------------------- Pairs */

PRIMITIVE(set_car) {
    need(self, argc, 2, 2);
    if (!dy_kind_p(a0, DY_KIND_PAIR)) dy_error("not a pair", a0);
    return ((DyPair *)DY_PTR(a0))->head = a1;
}

PRIMITIVE(set_cdr) {
    need(self, argc, 2, 2);
    if (!dy_kind_p(a0, DY_KIND_PAIR)) dy_error("not a pair", a0);
    return ((DyPair *)DY_PTR(a0))->tail = a1;
}

PRIMITIVE(pair_p) { need(self, argc, 1, 1); return DY_BOOL(dy_kind_p(a0, DY_KIND_PAIR)); }

/* ------------------------------------------------- Symbols and keywords */

static D string_bytes(D s, const char **bytes, size_t *size) {
    if (!dy_kind_p(s, DY_KIND_BYTE_STRING)) dy_error("not a <byte-string>", s);
    *bytes = ((DyByteString *)DY_PTR(s))->data;
    *size = DY_INT_VALUE(((DyByteString *)DY_PTR(s))->size);
    return s;
}

PRIMITIVE(intern_symbol) {
    need(self, argc, 1, 1);
    const char *b; size_t n;
    string_bytes(a0, &b, &n);
    return dy_intern_symbol(b, n);
}

PRIMITIVE(intern_keyword) {
    need(self, argc, 1, 1);
    const char *b; size_t n;
    string_bytes(a0, &b, &n);
    return dy_intern_keyword(b, n);
}

/* (%symbol-name symbol-or-keyword): a new string of its name */
PRIMITIVE(symbol_name) {
    need(self, argc, 1, 1);
    if (!dy_kind_p(a0, DY_KIND_SYMBOL) && !dy_kind_p(a0, DY_KIND_KEYWORD))
        dy_error("not a symbol or keyword", a0);
    DyByteString *s = DY_PTR(((DySymbol *)DY_PTR(a0))->name);
    return dy_make_byte_string(s->data, DY_INT_VALUE(s->size));
}

/* ------------------------------------------------------------- Hashing */

/* (%object-hash x): an identity hash.  The collector does not move
 * objects, so an address will do. */
PRIMITIVE(object_hash) {
    need(self, argc, 1, 1);
    uint64_t h = (uint64_t)a0 * 0x9E3779B97F4A7C15u;
    return DY_INT((intptr_t)(h >> 4) & DY_INT_MAX);
}

/* (%number-hash x): equal (=) numbers hash alike.  An integral float or
 * ratio hashes as the integer; others by their double's bits. */
PRIMITIVE(number_hash) {
    need(self, argc, 1, 1);
    if (DY_IS_INT(a0)) return DY_INT(DY_INT_VALUE(a0) & DY_INT_MAX);
    double x = dy_float_value(a0);
    if (x == floor(x) && fabs(x) < 0x1p62) return DY_INT((intptr_t)x & DY_INT_MAX);
    uint64_t bits;
    memcpy(&bits, &x, sizeof bits);
    return DY_INT((intptr_t)((bits * 0x9E3779B97F4A7C15u) >> 4) & DY_INT_MAX);
}

/* (%string-hash s): of the characters, case significant */
PRIMITIVE(string_hash) {
    need(self, argc, 1, 1);
    intptr_t n = vector_size(a0);
    uint64_t h = 14695981039346656037u;
    for (intptr_t i = 0; i < n; i++) {
        D c = dy_prim_vector_element_entry(self, a0, DY_INT(i), 0, 0, 2);
        h = (h ^ DY_CHAR_CODE(c)) * 1099511628211u;
    }
    return DY_INT((intptr_t)(h >> 2) & DY_INT_MAX);
}

/* -------------------------------------------------------------- Output */

static void write_bytes(D s) {
    if (dy_kind_p(s, DY_KIND_BYTE_STRING)) {
        DyByteString *b = DY_PTR(s);
        fwrite(b->data, 1, DY_INT_VALUE(b->size), stdout);
    } else if (dy_kind_p(s, DY_KIND_UNICODE_STRING)) {
        DyUnicodeString *u = DY_PTR(s);
        for (intptr_t i = 0; i < DY_INT_VALUE(u->size); i++) {
            uint32_t c = u->data[i];
            if (c < 0x80) putchar((int)c);
            else if (c < 0x800) { putchar(0xC0 | (c >> 6)); putchar(0x80 | (c & 0x3F)); }
            else if (c < 0x10000) {
                putchar(0xE0 | (c >> 12)); putchar(0x80 | ((c >> 6) & 0x3F));
                putchar(0x80 | (c & 0x3F));
            } else {
                putchar(0xF0 | (c >> 18)); putchar(0x80 | ((c >> 12) & 0x3F));
                putchar(0x80 | ((c >> 6) & 0x3F)); putchar(0x80 | (c & 0x3F));
            }
        }
    } else {
        dy_error("not a string", s);
    }
}

/* (%write-string s) */
PRIMITIVE(write_string) { need(self, argc, 1, 1); write_bytes(a0); return a0; }

/* (%write-char c), UTF-8 encoded */
PRIMITIVE(write_char) {
    need(self, argc, 1, 1);
    if (!DY_IS_CHAR(a0)) dy_error("not a character", a0);
    uint32_t c = DY_CHAR_CODE(a0);
    D s = dy_make_unicode_string(&c, 1);
    write_bytes(s);
    return a0;
}

/* (%number->string x): an integer or ratio in decimal; a float as the
 * shortest decimal that reads back as the same float, with a decimal
 * point -- and, for a single- or extended-float, the s0 or l0 of its
 * format (a double-float is the default format, as when reading). */
PRIMITIVE(number_to_string) {
    need(self, argc, 1, 1);
    return dy_number_string(a0);
}

D dy_number_string(D a0) {
    char buffer[80];
    if (DY_IS_INT(a0)) {
        snprintf(buffer, sizeof buffer, "%" PRIdPTR, DY_INT_VALUE(a0));
    } else if (dy_kind_p(a0, DY_KIND_RATIO)) {
        DyRatio *r = DY_PTR(a0);
        snprintf(buffer, sizeof buffer, "%" PRIdPTR "/%" PRIdPTR, DY_INT_VALUE(r->numerator),
                 DY_INT_VALUE(r->denominator));
    } else if (float_p(a0)) {
        bool single = dy_kind_p(a0, DY_KIND_SINGLE_FLOAT);
        double x = dy_float_value(a0);
        if (isnan(x)) strcpy(buffer, "+nan");
        else if (isinf(x)) strcpy(buffer, x > 0 ? "+infinity" : "-infinity");
        else {
            /* Fixed notation for ordinary magnitudes, else exponential;
             * either way the fewest digits that read back as X. */
            bool fixed = x == 0 || (fabs(x) >= 1e-5 && fabs(x) < 1e16);
            for (int digits = 1; digits <= 17; digits++) {
                if (fixed) snprintf(buffer, sizeof buffer, "%.*f", digits, x);
                else snprintf(buffer, sizeof buffer, "%.*g", digits, x);
                if (single ? (float)strtod(buffer, NULL) == (float)x : strtod(buffer, NULL) == x)
                    break;
            }
            if (fixed) {
                /* %f gives trailing zeros: keep one digit after the point. */
                char *point = strchr(buffer, '.');
                char *end = buffer + strlen(buffer) - 1;
                while (end > point + 1 && *end == '0') *end-- = '\0';
            }
            char *e = strchr(buffer, 'e');
            char exponent[16] = "0";
            if (e) {
                const char *x = e + 1;
                if (*x == '+') x++;
                bool negative = *x == '-';
                if (negative) x++;
                while (*x == '0' && x[1]) x++;      /* e+05 -> 5 */
                snprintf(exponent, sizeof exponent, "%s%s", negative ? "-" : "", x);
                *e = '\0';
            }
            if (!strchr(buffer, '.')) strcat(buffer, ".0");
            const char *marker = single ? "s" : dy_kind_p(a0, DY_KIND_EXTENDED_FLOAT) ? "l"
                               : e ? "e" : "";
            if (*marker) {
                strcat(buffer, marker);
                strcat(buffer, exponent);
            }
        }
    } else {
        dy_error("not a number", a0);
    }
    return dy_make_c_string(buffer);
}

/* -------------------------------------------------------------- Classes */

PRIMITIVE(class_name) {
    need(self, argc, 1, 1);
    if (!dy_kind_p(a0, DY_KIND_CLASS)) dy_error("not a class", a0);
    return ((DyClass *)DY_PTR(a0))->name;
}

PRIMITIVE(generic_name) {
    need(self, argc, 1, 1);
    if (!dy_kind_p(a0, DY_KIND_GENERIC)) dy_error("not a generic function", a0);
    return ((DyGeneric *)DY_PTR(a0))->name;
}

/* (%method-owner m): the generic function the method M is a method of,
 * or #f.  (%method-name m): the name of a predefined method, or #f. */
PRIMITIVE(method_owner) {
    need(self, argc, 1, 1);
    if (!dy_kind_p(a0, DY_KIND_FUNCTION)) dy_error("not a method", a0);
    D owner = ((DyFunction *)DY_PTR(a0))->owner;
    return owner == 0 ? DY_FALSE : owner;
}

PRIMITIVE(method_name) {
    need(self, argc, 1, 1);
    if (!dy_kind_p(a0, DY_KIND_FUNCTION)) dy_error("not a method", a0);
    const char *name = ((DyFunction *)DY_PTR(a0))->name;
    return name ? dy_intern_symbol(name, strlen(name)) : DY_FALSE;
}

PRIMITIVE(singleton_object) {
    need(self, argc, 1, 1);
    if (!dy_kind_p(a0, DY_KIND_SINGLETON)) dy_error("not a singleton", a0);
    return ((DySingleton *)DY_PTR(a0))->object;
}

PRIMITIVE(function_p) { need(self, argc, 1, 1); return DY_BOOL(dy_function_p(a0)); }

/* (%shallow-copy x): a new object with the same contents -- for pairs,
 * the runtime's vectors and strings, and instances of the program's
 * classes; anything else (numbers, symbols, ...) is itself. */
PRIMITIVE(shallow_copy) {
    need(self, argc, 1, 1);
    if (!DY_IS_POINTER(a0) || a0 == DY_TRUE || a0 == DY_FALSE || a0 == DY_EMPTY) return a0;
    DyWrapper *w = dy_wrapper_of(a0);
    switch (w->kind) {
    case DY_KIND_PAIR: return dy_pair(dy_head(a0), dy_tail(a0));
    case DY_KIND_SIMPLE_OBJECT_VECTOR:
    case DY_KIND_BYTE_STRING:
    case DY_KIND_UNICODE_STRING: {
        intptr_t n = vector_size(a0);
        D copy = w->kind == DY_KIND_SIMPLE_OBJECT_VECTOR ? dy_make_vector(n, DY_FALSE)
               : w->kind == DY_KIND_BYTE_STRING ? dy_make_byte_string("", 0)
               : dy_make_unicode_string(NULL, 0);
        if (w->kind == DY_KIND_BYTE_STRING)
            return dy_make_byte_string(((DyByteString *)DY_PTR(a0))->data, n);
        if (w->kind == DY_KIND_UNICODE_STRING)
            return dy_make_unicode_string(((DyUnicodeString *)DY_PTR(a0))->data, n);
        memcpy(((DyVector *)DY_PTR(copy))->data, ((DyVector *)DY_PTR(a0))->data, n * sizeof(D));
        return copy;
    }
    case DY_KIND_INSTANCE: {
        size_t bytes = sizeof(DyInstance) + w->size * sizeof(D);
        DyInstance *copy = dy_alloc(w, bytes);
        memcpy(copy, DY_PTR(a0), bytes);
        return DY_OBJ(copy);
    }
    default: return a0;
    }
}

/* ------------------------------------------- function-arguments (ch. 10) */

/* (function-arguments f): the number of required arguments, whether it
 * takes #rest, and its keywords (#f if it has no #key) -- for a function
 * the compiler knows the parameters of; a function written in C answers
 * 0, #t and #f, since it may take anything. */
PRIMITIVE(function_arguments) {
    need(self, argc, 1, 1);
    D info = DY_FALSE;
    if (dy_kind_p(a0, DY_KIND_FUNCTION)) info = ((DyFunction *)DY_PTR(a0))->info;
    else if (dy_kind_p(a0, DY_KIND_GENERIC)) info = ((DyGeneric *)DY_PTR(a0))->info;
    else dy_error("function-arguments: not a function", a0);
    DyThread *t = dy_thread;
    if (info == 0 || info == DY_FALSE) {
        t->mv[0] = DY_INT(0); t->mv[1] = DY_TRUE; t->mv[2] = DY_FALSE;
    } else {
        DyVector *v = DY_PTR(info);
        t->mv[0] = v->data[0]; t->mv[1] = v->data[1]; t->mv[2] = v->data[2];
    }
    t->mv_count = 3;
    return t->mv[0];
}

/* ------------------------------------------- Slot descriptors (ch. 11) */

static DySlotDescriptor *as_descriptor(D x) {
    if (!dy_kind_p(x, DY_KIND_SLOT_DESCRIPTOR)) dy_error("not a slot descriptor", x);
    return DY_PTR(x);
}

PRIMITIVE(slot_getter) { need(self, argc, 1, 1); return as_descriptor(a0)->getter; }
PRIMITIVE(slot_setter) { need(self, argc, 1, 1); return as_descriptor(a0)->setter; }
PRIMITIVE(slot_allocation) { need(self, argc, 1, 1); return as_descriptor(a0)->allocation; }
PRIMITIVE(init_keyword) { need(self, argc, 1, 1); return as_descriptor(a0)->init_keyword; }

PRIMITIVE(slot_type) {
    need(self, argc, 1, 1);
    D type = as_descriptor(a0)->type;
    return type == DY_UNBOUND ? dy_wrapper_object.class : type;
}

PRIMITIVE(init_function) {
    need(self, argc, 1, 1);
    D f = as_descriptor(a0)->init_function;
    return f == DY_UNBOUND ? DY_FALSE : f;
}

/* (init-value sd): the init-value and #t, or #f and #f */
PRIMITIVE(init_value) {
    need(self, argc, 1, 1);
    D v = as_descriptor(a0)->init_value;
    return two_values(v == DY_UNBOUND ? DY_FALSE : v, DY_BOOL(v != DY_UNBOUND));
}

/* The storage of the slot SD describes in INSTANCE */
static D *slot_place(D instance, DySlotDescriptor *sd) {
    DyClass *owner = DY_PTR(sd->owner);
    if (!dy_instance_p(instance, owner->instance_wrapper))
        dy_error("slot-value: the object does not have the slot", instance);
    DyWrapper *w = dy_wrapper_of(instance);
    switch (sd->kind) {
    case 0: {
        int32_t i = w->slot_offsets ? w->slot_offsets[sd->number] : -1;
        if (i < 0) dy_error("slot-value: the object does not have the slot", instance);
        return &((DyInstance *)DY_PTR(instance))->slots[i];
    }
    case 1: case 3: return sd->cell;
    case 2: return &sd->cell[w->id];
    default: dy_error("slot-value: a virtual slot has no storage", DY_OBJ(sd));
    }
}

PRIMITIVE(slot_value) {
    need(self, argc, 2, 2);
    D v = *slot_place(a0, as_descriptor(a1));
    if (v == DY_UNBOUND) dy_error("slot-value: the slot is not initialized", a0);
    return v;
}

PRIMITIVE(slot_value_setter) {
    need(self, argc, 3, 3);
    DySlotDescriptor *sd = as_descriptor(a1);
    if (sd->kind == 3) dy_error("slot-value: a constant slot cannot be set", a0);
    D *place = slot_place(a0, sd);
    if (sd->type != DY_UNBOUND) dy_check_type(a2, sd->type);
    return *place = a2;
}

/* ------------------------------------------------------------- Streams */

/* (%write-fd fd string-or-character): write to standard output (1) or
 * standard error (2). */
PRIMITIVE(write_fd) {
    need(self, argc, 2, 2);
    FILE *out = int_arg(a0) == 2 ? stderr : stdout;
    if (out == stderr) fflush(stdout);
    if (DY_IS_CHAR(a1)) {
        uint32_t c = DY_CHAR_CODE(a1);
        a1 = dy_make_unicode_string(&c, 1);
    }
    if (dy_kind_p(a1, DY_KIND_BYTE_STRING)) {
        DyByteString *b = DY_PTR(a1);
        fwrite(b->data, 1, DY_INT_VALUE(b->size), out);
    } else if (dy_kind_p(a1, DY_KIND_UNICODE_STRING)) {
        DyUnicodeString *u = DY_PTR(a1);
        for (intptr_t i = 0; i < DY_INT_VALUE(u->size); i++) {
            uint32_t c = u->data[i];
            if (c < 0x80) fputc((int)c, out);
            else if (c < 0x800) { fputc(0xC0 | (c >> 6), out); fputc(0x80 | (c & 0x3F), out); }
            else if (c < 0x10000) {
                fputc(0xE0 | (c >> 12), out); fputc(0x80 | ((c >> 6) & 0x3F), out);
                fputc(0x80 | (c & 0x3F), out);
            } else {
                fputc(0xF0 | (c >> 18), out); fputc(0x80 | ((c >> 12) & 0x3F), out);
                fputc(0x80 | ((c >> 6) & 0x3F), out); fputc(0x80 | (c & 0x3F), out);
            }
        }
    } else {
        dy_error("not a string or character", a1);
    }
    return a1;
}

/* (current-milliseconds): a monotonic clock, in milliseconds (an
 * extension, for timing). */
PRIMITIVE(current_milliseconds) {
    need(self, argc, 0, 0);
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return DY_INT((intptr_t)t.tv_sec * 1000 + t.tv_nsec / 1000000);
}
