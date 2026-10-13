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
#include <string.h>

D dy_make_byte_string(const char *bytes, size_t size) {
    if (size > (size_t)DY_INT_MAX) dy_error("string too large", 0);
    DyByteString *s = dy_alloc_atomic(&dy_wrapper_byte_string,
                                      sizeof(DyByteString) + size + 1);
    s->size = DY_INT(size);
    memcpy(s->data, bytes, size);
    s->data[size] = '\0';
    return DY_OBJ(s);
}

D dy_make_c_string(const char *s) { return dy_make_byte_string(s, strlen(s)); }

D dy_make_unicode_string(const uint32_t *chars, size_t size) {
    if (size > (size_t)DY_INT_MAX / 4) dy_error("string too large", 0);
    DyUnicodeString *s = dy_alloc_atomic(&dy_wrapper_unicode_string,
                                         sizeof(DyUnicodeString) + size * 4);
    s->size = DY_INT(size);
    memcpy(s->data, chars, size * 4);
    return DY_OBJ(s);
}

/* ------------------------------------------------------------ Symbols */

static inline unsigned char fold(unsigned char c) {
    return (c >= 'A' && c <= 'Z') ? c + ('a' - 'A') : c;
}

static uint64_t name_hash(const char *name, size_t size) {
    uint64_t h = 14695981039346656037u;            /* FNV-1a */
    for (size_t i = 0; i < size; i++) {
        h ^= fold((unsigned char)name[i]);
        h *= 1099511628211u;
    }
    return h ? h : 1;
}

static bool same_name(const DySymbol *symbol, const char *name, size_t size) {
    const DyByteString *s = DY_PTR(symbol->name);
    if ((size_t)DY_INT_VALUE(s->size) != size) return false;
    for (size_t i = 0; i < size; i++)
        if (fold((unsigned char)s->data[i]) != fold((unsigned char)name[i]))
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
    /* Uncollectable: the table is a root that holds every symbol. */
    t->entries = GC_MALLOC_UNCOLLECTABLE(t->capacity * sizeof(DySymbol *));
    if (!t->entries) dy_error("out of memory", 0);
    memset(t->entries, 0, t->capacity * sizeof(DySymbol *));
    t->count = 0;
    for (size_t i = 0; i < old_capacity; i++)
        if (old[i]) table_insert(t, old[i]);
    if (old) GC_FREE(old);
}

/* The symbol named NAME in T, or NULL.  T must be locked. */
static DySymbol *table_find(SymbolTable *t, const char *name, size_t size,
                            uint64_t hash) {
    if (!t->capacity) return NULL;
    size_t mask = t->capacity - 1;
    for (size_t i = hash & mask; t->entries[i]; i = (i + 1) & mask) {
        DySymbol *s = t->entries[i];
        if (s->hash == hash && same_name(s, name, size)) return s;
    }
    return NULL;
}

static void table_add(SymbolTable *t, DySymbol *symbol) {
    if ((t->count + 1) * 2 > t->capacity) table_grow(t);
    table_insert(t, symbol);
}

static D intern(SymbolTable *t, const char *name, size_t size) {
    uint64_t hash = name_hash(name, size);
    pthread_mutex_lock(&t->lock);
    DySymbol *s = table_find(t, name, size, hash);
    if (!s) {
        D string = dy_make_byte_string(name, size);
        s = dy_alloc(t->wrapper, sizeof(DySymbol));
        s->name = string;
        s->hash = hash;
        table_add(t, s);
    }
    pthread_mutex_unlock(&t->lock);
    return DY_OBJ(s);
}

D dy_intern_symbol(const char *name, size_t size) { return intern(&symbols, name, size); }
D dy_intern_keyword(const char *name, size_t size) { return intern(&keywords, name, size); }

void dy_intern_static_symbols(void) {
    for (size_t i = 0; i < dy_static_symbol_count; i++) {
        DySymbol *s = dy_static_symbols[i];
        SymbolTable *t = s->wrapper == &dy_wrapper_keyword ? &keywords : &symbols;
        const DyByteString *name = DY_PTR(s->name);
        size_t size = DY_INT_VALUE(name->size);
        s->hash = name_hash(name->data, size);
        pthread_mutex_lock(&t->lock);
        if (table_find(t, name->data, size, s->hash))
            dy_error("the compiler emitted a symbol twice", DY_OBJ(s));
        table_add(t, s);
        pthread_mutex_unlock(&t->lock);
    }
}
