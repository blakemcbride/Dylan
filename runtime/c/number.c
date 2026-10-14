/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Numbers (chapter 14), without bignums or complex numbers.
 *
 * The rules are Common Lisp's, as the manual says: rational results are
 * canonicalized (a ratio whose denominator is 1 is an integer); a float
 * with a rational gives a float of the float's format; two floats give the
 * wider format (single < double < extended); comparison of a float with a
 * rational is exact ("rational contagion for comparisons").
 *
 * Rational arithmetic is done exactly in 128 bits and the result reduced;
 * only a result whose numerator or denominator does not fit in 63 bits is
 * an error.  An <extended-float> is held as a C double.
 *
 * The 128-bit arithmetic is the C compiler's __int128 where it has one,
 * and otherwise (or with -DDY_NO_INT128) two 64-bit words in standard C.
 */

#include "dylan.h"
#include <math.h>

/* ------------------------------------------------- 128-bit integers */

#if defined(__SIZEOF_INT128__) && !defined(DY_NO_INT128)

typedef __int128 i128;

static inline i128 w_of(int64_t x) { return x; }
static inline i128 w_add(i128 a, i128 b) { return a + b; }
static inline i128 w_sub(i128 a, i128 b) { return a - b; }
static inline i128 w_mul(i128 a, i128 b) { return a * b; }
static inline i128 w_neg(i128 a) { return -a; }
static inline int w_cmp(i128 a, i128 b) { return (a > b) - (a < b); }
static inline int w_sign(i128 a) { return (a > 0) - (a < 0); }
/* A * 2^N, 0 <= N < 127 */
static inline i128 w_shl(i128 a, int n) { return a * ((i128)1 << n); }
/* floor(A / 2^N), 0 <= N < 127 */
static inline i128 w_sar(i128 a, int n) { return a >> n; }
/* A / B and A % B, truncating; B > 0 */
static inline i128 w_div(i128 a, i128 b) { return a / b; }
static inline i128 w_mod(i128 a, i128 b) { return a % b; }
static inline bool w_fits(i128 a, int64_t lo, int64_t hi) { return a >= lo && a <= hi; }
static inline int64_t w_int(i128 a) { return (int64_t)a; }

#else

/* Two's complement in two words */
typedef struct { uint64_t hi, lo; } i128;

static inline i128 w_of(int64_t x) {
    i128 r = { x < 0 ? UINT64_MAX : 0, (uint64_t)x };
    return r;
}
static inline i128 w_add(i128 a, i128 b) {
    i128 r = { a.hi + b.hi, a.lo + b.lo };
    r.hi += r.lo < a.lo;
    return r;
}
static inline i128 w_neg(i128 a) {
    i128 r = { ~a.hi, ~a.lo };
    return w_add(r, w_of(1));
}
static inline i128 w_sub(i128 a, i128 b) { return w_add(a, w_neg(b)); }
/* The full product of two unsigned 64-bit words */
static i128 mul64(uint64_t a, uint64_t b) {
    uint64_t a0 = a & 0xFFFFFFFFu, a1 = a >> 32, b0 = b & 0xFFFFFFFFu, b1 = b >> 32;
    uint64_t p00 = a0 * b0, p01 = a0 * b1, p10 = a1 * b0, p11 = a1 * b1;
    uint64_t mid = (p00 >> 32) + (p01 & 0xFFFFFFFFu) + (p10 & 0xFFFFFFFFu);
    i128 r = { p11 + (p01 >> 32) + (p10 >> 32) + (mid >> 32),
               (mid << 32) | (p00 & 0xFFFFFFFFu) };
    return r;
}
static inline i128 w_mul(i128 a, i128 b) {
    i128 r = mul64(a.lo, b.lo);
    r.hi += a.hi * b.lo + a.lo * b.hi;
    return r;
}
static inline int w_sign(i128 a) {
    if ((int64_t)a.hi < 0) return -1;
    return (a.hi | a.lo) != 0;
}
static inline int w_cmp(i128 a, i128 b) {
    if (a.hi != b.hi) return (int64_t)a.hi < (int64_t)b.hi ? -1 : 1;
    return (a.lo > b.lo) - (a.lo < b.lo);
}
static inline i128 w_shl(i128 a, int n) {
    if (n == 0) return a;
    i128 r;
    if (n >= 64) { r.hi = a.lo << (n - 64); r.lo = 0; }
    else { r.hi = (a.hi << n) | (a.lo >> (64 - n)); r.lo = a.lo << n; }
    return r;
}
static inline i128 w_sar(i128 a, int n) {
    if (n == 0) return a;
    uint64_t fill = (int64_t)a.hi < 0 ? UINT64_MAX : 0;
    i128 r;
    if (n >= 64) {
        r.hi = fill;
        r.lo = n == 64 ? a.hi : (a.hi >> (n - 64)) | (fill << (128 - n));
    } else {
        r.hi = (a.hi >> n) | (fill << (64 - n));
        r.lo = (a.lo >> n) | (a.hi << (64 - n));
    }
    return r;
}
/* Unsigned A / B, remainder in *R; B != 0 */
static i128 udivmod(i128 a, i128 b, i128 *r) {
    if (a.hi == 0 && b.hi == 0) {
        i128 q = { 0, a.lo / b.lo };
        r->hi = 0; r->lo = a.lo % b.lo;
        return q;
    }
    i128 q = { 0, 0 }, rem = { 0, 0 };
    for (int i = 127; i >= 0; i--) {
        rem = w_shl(rem, 1);
        rem.lo |= (i >= 64 ? a.hi >> (i - 64) : a.lo >> i) & 1;
        if (rem.hi > b.hi || (rem.hi == b.hi && rem.lo >= b.lo)) {
            rem = w_sub(rem, b);
            if (i >= 64) q.hi |= (uint64_t)1 << (i - 64);
            else q.lo |= (uint64_t)1 << i;
        }
    }
    *r = rem;
    return q;
}
static inline i128 w_div(i128 a, i128 b) {
    i128 r;
    bool negative = w_sign(a) < 0;
    i128 q = udivmod(negative ? w_neg(a) : a, b, &r);
    return negative ? w_neg(q) : q;
}
static inline i128 w_mod(i128 a, i128 b) {
    i128 r;
    bool negative = w_sign(a) < 0;
    udivmod(negative ? w_neg(a) : a, b, &r);
    return negative ? w_neg(r) : r;
}
static inline bool w_fits(i128 a, int64_t lo, int64_t hi) {
    return w_cmp(a, w_of(lo)) >= 0 && w_cmp(a, w_of(hi)) <= 0;
}
static inline int64_t w_int(i128 a) { return (int64_t)a.lo; }

#endif

enum { N_NOT = 0, N_INT, N_RATIO, N_SINGLE, N_DOUBLE, N_EXTENDED };

static int number_kind(D x) {
    if (DY_IS_INT(x)) return N_INT;
    if (!DY_IS_POINTER(x)) return N_NOT;
    switch (((DyHeader *)DY_PTR(x))->wrapper->kind) {
    case DY_KIND_RATIO: return N_RATIO;
    case DY_KIND_SINGLE_FLOAT: return N_SINGLE;
    case DY_KIND_DOUBLE_FLOAT: return N_DOUBLE;
    case DY_KIND_EXTENDED_FLOAT: return N_EXTENDED;
    default: return N_NOT;
    }
}

bool dy_number_p(D x) { return number_kind(x) != N_NOT; }

static _Noreturn void not_a_number(D x) { dy_error("not a number", x); }

/* --------------------------------------------------------- Rationals */

/* The greatest common divisor of A >= 0 and B > 0 */
static i128 gcd128(i128 a, i128 b) {
    while (w_sign(b) != 0) { i128 t = w_mod(a, b); a = b; b = t; }
    return a;
}

/* The canonical rational N/D.  |N| and |D| are below 2^126. */
static D make_rational(i128 n, i128 d) {
    if (w_sign(d) == 0) dy_division_by_zero();
    if (w_sign(d) < 0) { n = w_neg(n); d = w_neg(d); }
    i128 g = gcd128(w_sign(n) < 0 ? w_neg(n) : n, d);
    if (w_cmp(g, w_of(1)) > 0) { n = w_div(n, g); d = w_div(d, g); }
    if (!w_fits(n, DY_INT_MIN, DY_INT_MAX) || !w_fits(d, 1, DY_INT_MAX))
        dy_integer_overflow();
    if (w_cmp(d, w_of(1)) == 0) return DY_INT((intptr_t)w_int(n));
    DyRatio *r = dy_alloc_atomic(&dy_wrapper_ratio, sizeof(DyRatio));
    r->numerator = DY_INT((intptr_t)w_int(n));
    r->denominator = DY_INT((intptr_t)w_int(d));
    return DY_OBJ(r);
}

D dy_make_ratio(D numerator, D denominator) {
    if (!DY_IS_INT(numerator)) dy_error("not an integer", numerator);
    if (!DY_IS_INT(denominator)) dy_error("not an integer", denominator);
    return make_rational(w_of(DY_INT_VALUE(numerator)), w_of(DY_INT_VALUE(denominator)));
}

static void rational_parts(D x, i128 *n, i128 *d) {
    if (DY_IS_INT(x)) { *n = w_of(DY_INT_VALUE(x)); *d = w_of(1); return; }
    DyRatio *r = DY_PTR(x);
    *n = w_of(DY_INT_VALUE(r->numerator));
    *d = w_of(DY_INT_VALUE(r->denominator));
}

/* ------------------------------------------------------------- Floats */

D dy_make_single_float(float value) {
    DySingleFloat *f = dy_alloc_atomic(&dy_wrapper_single_float, sizeof(DySingleFloat));
    f->value = value;
    return DY_OBJ(f);
}

D dy_make_double_float(double value) {
    DyDoubleFloat *f = dy_alloc_atomic(&dy_wrapper_double_float, sizeof(DyDoubleFloat));
    f->value = value;
    return DY_OBJ(f);
}

D dy_make_extended_float(double value) {
    DyDoubleFloat *f = dy_alloc_atomic(&dy_wrapper_extended_float, sizeof(DyDoubleFloat));
    f->value = value;
    return DY_OBJ(f);
}

static D make_float(int kind, double value) {
    switch (kind) {
    case N_SINGLE: return dy_make_single_float((float)value);
    case N_DOUBLE: return dy_make_double_float(value);
    default: return dy_make_extended_float(value);
    }
}

/* X, any real, as a double.  A ratio is divided in long double, which
 * holds both its parts exactly on x86-64. */
static double to_double(D x, int kind) {
    switch (kind) {
    case N_INT: return (double)DY_INT_VALUE(x);
    case N_RATIO: {
        DyRatio *r = DY_PTR(x);
        return (double)((long double)DY_INT_VALUE(r->numerator) /
                        (long double)DY_INT_VALUE(r->denominator));
    }
    case N_SINGLE: return ((DySingleFloat *)DY_PTR(x))->value;
    default: return ((DyDoubleFloat *)DY_PTR(x))->value;
    }
}

double dy_float_value(D x) {
    int kind = number_kind(x);
    if (kind == N_NOT) not_a_number(x);
    return to_double(x, kind);
}

/* --------------------------------------------------------- Arithmetic */

enum { OP_ADD, OP_SUB, OP_MUL, OP_DIV };

static D arith(int op, D a, D b) {
    int ka = number_kind(a), kb = number_kind(b);
    if (ka == N_NOT) not_a_number(a);
    if (kb == N_NOT) not_a_number(b);
    if (ka >= N_SINGLE || kb >= N_SINGLE) {
        /* Float contagion: the wider format. */
        int kind = ka > kb ? ka : kb;
        double x = to_double(a, ka), y = to_double(b, kb), r;
        switch (op) {
        case OP_ADD: r = x + y; break;
        case OP_SUB: r = x - y; break;
        case OP_MUL: r = x * y; break;
        default:
            if (y == 0.0) dy_division_by_zero();
            r = x / y;
        }
        return make_float(kind, r);
    }
    if (ka == N_INT && kb == N_INT && op != OP_DIV) {
        switch (op) {
        case OP_ADD: return dy_int_add(a, b);
        case OP_SUB: return dy_int_sub(a, b);
        default: return dy_int_mul(a, b);
        }
    }
    i128 an, ad, bn, bd;
    rational_parts(a, &an, &ad);
    rational_parts(b, &bn, &bd);
    switch (op) {
    case OP_ADD: return make_rational(w_add(w_mul(an, bd), w_mul(bn, ad)), w_mul(ad, bd));
    case OP_SUB: return make_rational(w_sub(w_mul(an, bd), w_mul(bn, ad)), w_mul(ad, bd));
    case OP_MUL: return make_rational(w_mul(an, bn), w_mul(ad, bd));
    default: return make_rational(w_mul(an, bd), w_mul(ad, bn));
    }
}

D dy_add(D a, D b) { return arith(OP_ADD, a, b); }
D dy_sub(D a, D b) { return arith(OP_SUB, a, b); }
D dy_mul(D a, D b) { return arith(OP_MUL, a, b); }
D dy_div(D a, D b) { return arith(OP_DIV, a, b); }

D dy_negate(D a) {
    switch (number_kind(a)) {
    case N_INT: return dy_int_negate(a);
    case N_RATIO: {
        DyRatio *r = DY_PTR(a);
        return make_rational(w_neg(w_of(DY_INT_VALUE(r->numerator))),
                             w_of(DY_INT_VALUE(r->denominator)));
    }
    case N_SINGLE: return dy_make_single_float(-((DySingleFloat *)DY_PTR(a))->value);
    case N_DOUBLE: return dy_make_double_float(-((DyDoubleFloat *)DY_PTR(a))->value);
    case N_EXTENDED: return dy_make_extended_float(-((DyDoubleFloat *)DY_PTR(a))->value);
    default: not_a_number(a);
    }
}

/* --------------------------------------------------------- Comparison */

/* The sign of N/D - F, exactly, for D > 0 and F finite. */
static int compare_rational_double(i128 n, i128 d, double f) {
    if (f == 0.0) return w_sign(n);
    /* |N/D| < 2^62, so a float at least 2^63 in magnitude decides. */
    if (fabs(f) >= 0x1p63) return f > 0 ? -1 : 1;
    /* A nonzero rational is at least 2^-62 in magnitude. */
    if (w_sign(n) != 0 && fabs(f) < 0x1p-64) return w_sign(n);
    if (w_sign(n) == 0) return f > 0 ? -1 : 1;
    /* F = M * 2^E, M an integer below 2^53 in magnitude.  Compare N with
     * M * D * 2^E; |M * D| < 2^115.  (Shifts of negative numbers are
     * written as multiplications, which C defines.) */
    int ex;
    double fr = frexp(f, &ex);
    i128 m = w_of((int64_t)ldexp(fr, 53));
    int e = ex - 53;                        /* -117 <= e <= 10 here */
    i128 x = w_mul(m, d);
    if (e >= 0)
        return w_sign(w_sub(n, w_shl(x, e))); /* |x 2^e| < 2^125 */
    /* Compare N * 2^s with X, s = -E.  If s <= 64, N * 2^s < 2^126. */
    int s = -e;
    if (s <= 64) return w_sign(w_sub(w_shl(n, s), x));
    /* Otherwise compare N * 2^64 with X / 2^t, t = s - 64: with
     * Q = floor(X / 2^t) and R = X - Q * 2^t, 0 <= R < 2^t,
     * N * 2^s - X = (N * 2^64 - Q) * 2^t - R. */
    int t = s - 64;
    i128 q = w_sar(x, t);                   /* floor */
    i128 r = w_sub(x, w_shl(q, t));
    i128 l = w_shl(n, 64);
    if (w_cmp(l, q) != 0) return w_cmp(l, q);
    return w_sign(r) == 0 ? 0 : -1;
}

int dy_compare(D a, D b) {
    int ka = number_kind(a), kb = number_kind(b);
    if (ka == N_NOT) not_a_number(a);
    if (kb == N_NOT) not_a_number(b);
    if (ka == N_INT && kb == N_INT)
        return ((intptr_t)a > (intptr_t)b) - ((intptr_t)a < (intptr_t)b);
    if (ka < N_SINGLE && kb < N_SINGLE) {
        i128 an, ad, bn, bd;
        rational_parts(a, &an, &ad);
        rational_parts(b, &bn, &bd);
        return w_sign(w_sub(w_mul(an, bd), w_mul(bn, ad)));
    }
    if (ka >= N_SINGLE && kb >= N_SINGLE) {
        double x = to_double(a, ka), y = to_double(b, kb);
        if (isnan(x) || isnan(y)) return 2;
        return (x > y) - (x < y);
    }
    /* A rational and a float: exactly. */
    bool flip = ka >= N_SINGLE;
    D r = flip ? b : a, f = flip ? a : b;
    double y = to_double(f, flip ? ka : kb);
    if (isnan(y)) return 2;
    int c;
    if (isinf(y)) c = y > 0 ? -1 : 1;
    else {
        i128 n, d;
        rational_parts(r, &n, &d);
        c = compare_rational_double(n, d, y);
    }
    return flip ? -c : c;
}

bool dy_number_equal(D a, D b) { return dy_compare(a, b) == 0; }
bool dy_number_less(D a, D b) { return dy_compare(a, b) == -1; }

/* ---------------------------------------------------- Integer division */

static void check_divisor(D a, D b) {
    if (!DY_IS_INT(a)) dy_error("not an integer", a);
    if (!DY_IS_INT(b)) dy_error("not an integer", b);
    if (b == DY_INT(0)) dy_division_by_zero();
}

/* Truncating division; Q * Y + R = X.  Only DY_INT_MIN / -1 overflows. */
static void truncate2(intptr_t x, intptr_t y, intptr_t *q, intptr_t *r) {
    *q = x / y;
    *r = x % y;
}

D dy_int_truncate(D a, D b, D *remainder) {
    check_divisor(a, b);
    intptr_t q, r;
    truncate2(DY_INT_VALUE(a), DY_INT_VALUE(b), &q, &r);
    *remainder = DY_INT(r);
    return dy_make_int(q);
}

D dy_int_floor(D a, D b, D *remainder) {
    check_divisor(a, b);
    intptr_t y = DY_INT_VALUE(b), q, r;
    truncate2(DY_INT_VALUE(a), y, &q, &r);
    if (r != 0 && ((r < 0) != (y < 0))) { q -= 1; r += y; }
    *remainder = DY_INT(r);
    return dy_make_int(q);
}

D dy_int_ceiling(D a, D b, D *remainder) {
    check_divisor(a, b);
    intptr_t y = DY_INT_VALUE(b), q, r;
    truncate2(DY_INT_VALUE(a), y, &q, &r);
    if (r != 0 && ((r < 0) == (y < 0))) { q += 1; r -= y; }
    *remainder = DY_INT(r);
    return dy_make_int(q);
}

/* To the nearest integer; a tie goes to the even one. */
D dy_int_round(D a, D b, D *remainder) {
    check_divisor(a, b);
    intptr_t y = DY_INT_VALUE(b), q, r;
    truncate2(DY_INT_VALUE(a), y, &q, &r);
    if (r != 0) {
        /* Compare 2|R| with |Y|, in 128 bits. */
        i128 twice = w_shl(w_of(r < 0 ? -r : r), 1);
        i128 ay = y < 0 ? w_neg(w_of(y)) : w_of(y);
        int step = (r < 0) == (y < 0) ? 1 : -1;  /* sign of the true quotient */
        int c = w_cmp(twice, ay);
        if (c > 0 || (c == 0 && (q & 1))) { q += step; r -= step * y; }
    }
    *remainder = DY_INT(r);
    return dy_make_int(q);
}
