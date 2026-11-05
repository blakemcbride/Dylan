/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Strings, symbols and keywords.
 *
 * Symbols and keywords are not case sensitive, but keep the case of their
 * first spelling (chapter 15).  Each kind has one table, an open-addressed
 * hash table keyed by the name folded to lower case, holding every symbol
 * (keyword) that exists: the program's own, which the compiler emits as
 * static objects and dy_init interns, and those made at run time by as.
 * A mutex guards each table, so any thread may intern.
 */

#include "dylan.h"
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>
#include <wctype.h>

D dy_make_byte_string(const char *bytes, size_t size) {
    if (size > (size_t)DY_INT_MAX) dy_error("string too large", 0);
    DyByteString *s = dy_alloc_atomic_sized(&dy_wrapper_byte_string,
                                            sizeof(DyByteString) + size + 1, DY_INT(size));
    memcpy(s->data, bytes, size);
    s->data[size] = '\0';
    return DY_OBJ(s);
}

D dy_make_c_string(const char *s) { return dy_make_byte_string(s, strlen(s)); }

D dy_make_unicode_string(const uint32_t *chars, size_t size) {
    if (size > (size_t)DY_INT_MAX / 4) dy_error("string too large", 0);
    DyUnicodeString *s = dy_alloc_atomic_sized(&dy_wrapper_unicode_string,
                                               sizeof(DyUnicodeString) + size * 4,
                                               DY_INT(size));
    memcpy(s->data, chars, size * 4);
    return DY_OBJ(s);
}

D dy_make_string_from_utf8(const char *bytes, size_t size) {
    uint32_t *chars = malloc((size ? size : 1) * sizeof(uint32_t));
    if (!chars) dy_error("out of memory", 0);
    const unsigned char *b = (const unsigned char *)bytes;
    size_t n = 0;
    bool wide = false;
    for (size_t i = 0; i < size; ) {
        uint32_t c = b[i];
        /* the continuation bytes a lead byte says follow */
        size_t more = c >= 0xF8 ? 0 : c >= 0xF0 ? 3 : c >= 0xE0 ? 2 : c >= 0xC0 ? 1 : 0;
        bool ok = more > 0 && i + more < size;
        uint32_t v = c & (0x3F >> more);
        for (size_t k = 1; ok && k <= more; k++) {
            if ((b[i + k] & 0xC0) != 0x80) ok = false;
            else v = (v << 6) | (b[i + k] & 0x3F);
        }
        if (ok) { c = v; i += more + 1; }
        else i++;
        if (c > 0xFF) wide = true;
        chars[n++] = c;
    }
    D string;
    if (wide) {
        string = dy_make_unicode_string(chars, n);
    } else {
        char *narrow = malloc(n ? n : 1);
        if (!narrow) dy_error("out of memory", 0);
        for (size_t i = 0; i < n; i++) narrow[i] = (char)chars[i];
        string = dy_make_byte_string(narrow, n);
        free(narrow);
    }
    free(chars);
    return string;
}

/* ------------------------------------------------------------ Symbols */

/* The case of a character, as Common Lisp's char-downcase and
 * char-upcase give it: only between the two of a pair of cases that map
 * to each other, so that the Kelvin sign, say, is not taken for a k. */
uint32_t dy_char_downcase(uint32_t c) {
    if (c < 0x80) return (c >= 'A' && c <= 'Z') ? c + ('a' - 'A') : c;
    wint_t l = towlower((wint_t)c);
    return (l != (wint_t)c && towupper(l) == towupper((wint_t)c)) ? (uint32_t)l : c;
}

uint32_t dy_char_upcase(uint32_t c) {
    if (c < 0x80) return (c >= 'a' && c <= 'z') ? c - ('a' - 'A') : c;
    wint_t u = towupper((wint_t)c);
    return (u != (wint_t)c && towlower(u) == towlower((wint_t)c)) ? (uint32_t)u : c;
}

/* A name to look up: the characters of a <byte-string> or of a
 * <unicode-string>, or bytes from C. */
typedef struct {
    const unsigned char *narrow;       /* or NULL, and then WIDE           */
    const uint32_t *wide;
    size_t size;
} Name;

static Name name_of(D string) {
    Name n = { NULL, NULL, (size_t)DY_INT_VALUE(((DyByteString *)DY_PTR(string))->size) };
    if (dy_kind_p(string, DY_KIND_BYTE_STRING))
        n.narrow = (const unsigned char *)((DyByteString *)DY_PTR(string))->data;
    else
        n.wide = ((DyUnicodeString *)DY_PTR(string))->data;
    return n;
}

static inline uint32_t name_char(const Name *n, size_t i) {
    return n->narrow ? n->narrow[i] : n->wide[i];
}

static uint64_t name_hash(const Name *n) {
    uint64_t h = 14695981039346656037u;            /* FNV-1a */
    for (size_t i = 0; i < n->size; i++) {
        h ^= dy_char_downcase(name_char(n, i));
        h *= 1099511628211u;
    }
    return h ? h : 1;
}

static bool same_name(const DySymbol *symbol, const Name *n) {
    Name m = name_of(symbol->name);
    if (m.size != n->size) return false;
    for (size_t i = 0; i < n->size; i++)
        if (dy_char_downcase(name_char(&m, i)) != dy_char_downcase(name_char(n, i)))
            return false;
    return true;
}

typedef struct {
    DySymbol **entries;                /* NULL where empty               */
    size_t capacity;                   /* a power of two                 */
    size_t count;
    pthread_mutex_t lock;
    DyWrapper *wrapper;
} SymbolTable;

static SymbolTable symbols = { NULL, 0, 0, PTHREAD_MUTEX_INITIALIZER, &dy_wrapper_symbol };
static SymbolTable keywords = { NULL, 0, 0, PTHREAD_MUTEX_INITIALIZER, &dy_wrapper_keyword };

static void table_insert(SymbolTable *t, DySymbol *symbol) {
    size_t mask = t->capacity - 1;
    size_t i = symbol->hash & mask;
    while (t->entries[i]) i = (i + 1) & mask;
    t->entries[i] = symbol;
    t->count++;
}

static void table_grow(SymbolTable *t) {
    DySymbol **old = t->entries;
    size_t old_capacity = t->capacity;
    t->capacity = old_capacity ? old_capacity * 2 : 256;
    /* A root: the table holds every symbol. */
    t->entries = dy_alloc_root(t->capacity * sizeof(DySymbol *));
    memset(t->entries, 0, t->capacity * sizeof(DySymbol *));
    t->count = 0;
    for (size_t i = 0; i < old_capacity; i++)
        if (old[i]) table_insert(t, old[i]);
    if (old) dy_free_root(old);
}

/* The symbol named N in T, or NULL.  T must be locked. */
static DySymbol *table_find(SymbolTable *t, const Name *n, uint64_t hash) {
    if (!t->capacity) return NULL;
    size_t mask = t->capacity - 1;
    for (size_t i = hash & mask; t->entries[i]; i = (i + 1) & mask) {
        DySymbol *s = t->entries[i];
        if (s->hash == hash && same_name(s, n)) return s;
    }
    return NULL;
}

static void table_add(SymbolTable *t, DySymbol *symbol) {
    if ((t->count + 1) * 2 > t->capacity) table_grow(t);
    table_insert(t, symbol);
}

/* A new string of N's characters: a <byte-string> if each fits in a
 * byte, else a <unicode-string>.  They are copied out before anything is
 * allocated, so that N may be in a string the collector could move. */
static D copy_name(const Name *n) {
    bool wide = false;
    for (size_t i = 0; i < n->size && !wide; i++) wide = name_char(n, i) > 0xFF;
    D string;
    if (wide) {
        uint32_t *chars = malloc((n->size ? n->size : 1) * sizeof(uint32_t));
        if (!chars) dy_error("out of memory", 0);
        memcpy(chars, n->wide, n->size * sizeof(uint32_t));
        string = dy_make_unicode_string(chars, n->size);
        free(chars);
    } else {
        char *bytes = malloc(n->size ? n->size : 1);
        if (!bytes) dy_error("out of memory", 0);
        for (size_t i = 0; i < n->size; i++) bytes[i] = (char)name_char(n, i);
        string = dy_make_byte_string(bytes, n->size);
        free(bytes);
    }
    return string;
}

/* The program's top-level forms begun so far.  A static symbol whose form
 * is later has not been read yet, as it were, in the read-eval-print
 * loop: the program has it, so that it is the one symbol of its name, but
 * as, finding it, makes it then, with as's spelling. */
static _Atomic intptr_t forms_begun;

void dy_symbols_through(intptr_t form) {
    atomic_store_explicit(&forms_begun, form, memory_order_release);
}

/* AS: for as, which makes a symbol not yet read with its own spelling */
static D intern(SymbolTable *t, const Name *n, bool as) {
    uint64_t hash = name_hash(n);
    pthread_mutex_lock(&t->lock);
    DySymbol *s = table_find(t, n, hash);
    if (!s) {
        D string = copy_name(n);
        s = dy_alloc(t->wrapper, sizeof(DySymbol));
        s->name = string;
        s->hash = hash;
        s->form = 0;
        table_add(t, s);
    } else if (as && s->form > atomic_load_explicit(&forms_begun, memory_order_acquire)) {
        s->name = copy_name(n);
        s->form = 0;
    }
    pthread_mutex_unlock(&t->lock);
    return DY_OBJ(s);
}

/* NAME, SIZE bytes, holds the characters of the first 256 code points */
D dy_intern_symbol(const char *name, size_t size) {
    Name n = { (const unsigned char *)name, NULL, size };
    return intern(&symbols, &n, false);
}

D dy_intern_keyword(const char *name, size_t size) {
    Name n = { (const unsigned char *)name, NULL, size };
    return intern(&keywords, &n, false);
}

/* STRING is a <byte-string> or a <unicode-string>.  AS: for as. */
D dy_intern_symbol_string(D string, bool as) {
    Name n = name_of(string);
    return intern(&symbols, &n, as);
}

D dy_intern_keyword_string(D string, bool as) {
    Name n = name_of(string);
    return intern(&keywords, &n, as);
}

/* A new string of the name of SYMBOL, a symbol or keyword */
D dy_symbol_name(D symbol) {
    Name n = name_of(((DySymbol *)DY_PTR(symbol))->name);
    return copy_name(&n);
}

/* The name of SYMBOL as UTF-8, in memory of the collector's */
const char *dy_symbol_utf8(D symbol) {
    char *buffer = NULL;
    size_t size = 0;
    FILE *out = open_memstream(&buffer, &size);
    dy_put_string(out, ((DySymbol *)DY_PTR(symbol))->name);
    fclose(out);
    char *text = dy_alloc_bytes(size + 1);
    memcpy(text, buffer, size + 1);
    free(buffer);
    return text;
}

void dy_intern_static_symbols(void) {
    for (size_t i = 0; i < dy_static_symbol_count; i++) {
        DySymbol *s = dy_static_symbols[i];
        SymbolTable *t = s->wrapper == &dy_wrapper_keyword ? &keywords : &symbols;
        Name n = name_of(s->name);
        s->hash = name_hash(&n);
        pthread_mutex_lock(&t->lock);
        if (table_find(t, &n, s->hash))
            dy_error("the compiler emitted a symbol twice", DY_OBJ(s));
        table_add(t, s);
        pthread_mutex_unlock(&t->lock);
    }
}
