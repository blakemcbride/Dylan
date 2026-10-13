/* Copyright 2026 Blake McBride.  See ../../LICENSE.
 *
 * Non-local exits, cleanups and conditions (chapters 8 and 13).
 *
 * Each thread has a dynamic chain of frames, innermost first: exit points
 * (bind-exit), cleanups (unwind-protect) and handlers (handler-bind).  The
 * frames live on the C stack of the functions here that establish them.
 * An exit procedure stores its arguments as the values to return, runs
 * the cleanups of the frames it leaves, innermost first, and longjmps to
 * its frame.
 *
 * signal looks through the chain for a handler whose type the condition
 * matches and whose test, if it has one, is true; calls it with the
 * condition and a next-handler that continues the search below it; and if
 * none applies calls default-handler.  The conditions themselves, and
 * error, signal and the rest, are written in Dylan in the library
 * (runtime/dylan/conditions.dyl), which gives the runtime, at startup, the
 * functions it uses to signal the errors it detects itself.
 */

#include "primitive.h"
#include <stdlib.h>
#include <string.h>

static D error_hook, type_error_hook, default_handler;

static void push_frame(DyFrame *f, int kind) {
    DyThread *t = dy_thread;
    f->kind = kind;
    f->prev = t->frames;
    f->serial = ++t->frame_serial;
    f->error_depth = t->error_depth;
    t->frames = f;
}

static bool in_chain(DyFrame *f, uint64_t serial) {
    for (DyFrame *g = dy_thread->frames; g; g = g->prev)
        if (g == f) return g->serial == serial;
    return false;
}

/* ------------------------------------------------- Values in flight */

typedef struct { intptr_t count; D primary; D *values; } Saved;

static Saved save_values(D primary) {
    DyThread *t = dy_thread;
    Saved s = { t->mv_count, primary, NULL };
    if (s.count != 1 && s.count > 0) {
        s.values = GC_MALLOC(s.count * sizeof(D));
        memcpy(s.values, t->mv, s.count * sizeof(D));
    }
    return s;
}

static D restore_values(Saved s) {
    DyThread *t = dy_thread;
    t->mv_count = s.count;
    if (s.values) memcpy(t->mv, s.values, s.count * sizeof(D));
    return s.primary;
}

/* ---------------------------------------------------------------- Exits */

/* Leave every frame above TARGET, running cleanups, then jump to it.  The
 * values to return are in the thread. */
_Noreturn void dy_unwind_to(DyFrame *target) {
    DyThread *t = dy_thread;
    Saved s = save_values(t->mv_count > 0 ? t->mv[0] : DY_FALSE);
    while (t->frames != target) {
        DyFrame *f = t->frames;
        t->frames = f->prev;
        if (f->kind == DY_FRAME_CLEANUP) {
            DY_CALL(f->a, 0, 0, 0, 0, 0);
            restore_values(s);
        }
    }
    if (s.count == 1) t->mv[0] = s.primary;
    /* longjmp, not _longjmp, which ThreadSanitizer does not see (the
     * jump buffer, made by _setjmp, holds no signal mask to restore). */
    longjmp(target->jump, 1);
}

/* env[0]: the frame; env[1]: its serial number */
static D exit_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    DyFunction *e = DY_PTR(self);
    DyFrame *target = (DyFrame *)e->env[0];
    if (!in_chain(target, (uint64_t)DY_INT_VALUE(e->env[1])))
        dy_error("an exit procedure was called after its bind-exit returned", 0);
    DyThread *t = dy_thread;
    D in_registers[4] = { a0, a1, a2, a3 };
    for (intptr_t i = 0; i < argc; i++)
        t->mv[i] = i < 4 ? in_registers[i] : t->args[i - 4];
    t->mv_count = argc;
    dy_unwind_to(target);
}

D dy_call_with_exit(D body) {
    DyFrame f;
    push_frame(&f, DY_FRAME_EXIT);
    D exit = dy_make_closure(exit_entry, 2);
    ((DyFunction *)DY_PTR(exit))->env[0] = (D)&f;
    ((DyFunction *)DY_PTR(exit))->env[1] = DY_INT(f.serial);
    if (_setjmp(f.jump) == 0) {
        D r = DY_CALL(body, exit, 0, 0, 0, 1);
        dy_thread->frames = f.prev;
        return r;
    }
    DyThread *t = dy_thread;
    t->frames = f.prev;
    t->error_depth = f.error_depth;
    return t->mv_count > 0 ? t->mv[0] : DY_FALSE;
}

D dy_unwind_protect(D protected, D cleanup) {
    DyFrame f;
    f.a = cleanup;
    push_frame(&f, DY_FRAME_CLEANUP);
    D r = DY_CALL(protected, 0, 0, 0, 0, 0);
    dy_thread->frames = f.prev;
    Saved s = save_values(r);
    DY_CALL(cleanup, 0, 0, 0, 0, 0);
    return restore_values(s);
}

D dy_handler_bind(D type, D function, D test, D description, D thunk) {
    DyFrame f;
    f.a = type; f.b = function; f.c = test; f.d = description;
    push_frame(&f, DY_FRAME_HANDLER);
    D r = DY_CALL(thunk, 0, 0, 0, 0, 0);
    dy_thread->frames = f.prev;
    return r;
}

/* ------------------------------------------------------------ Signals */

/* The first handler from START on that applies to CONDITION, or NULL. */
static DyFrame *find_handler(D condition, DyFrame *start) {
    for (DyFrame *f = start; f; f = f->prev) {
        if (f->kind != DY_FRAME_HANDLER) continue;
        if (!dy_specializer_matches(condition, f->a)) continue;
        if (f->c != DY_FALSE && DY_CALL(f->c, condition, 0, 0, 0, 1) == DY_FALSE) continue;
        return f;
    }
    return NULL;
}

/* env[0]: the condition; env[1]: the frame to search from (0 for none);
 * env[2]: its serial number */
static D next_handler_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc);

static D next_handler(D condition, DyFrame *start) {
    D next = dy_make_closure(next_handler_entry, 3);
    DyFunction *n = DY_PTR(next);
    n->env[0] = condition;
    n->env[1] = (D)start;
    n->env[2] = DY_INT(start ? start->serial : 0);
    return next;
}

/* Call the first handler from START on that applies to CONDITION, with
 * a next-handler; or, if none does, default-handler.  A macro, since it
 * makes tail calls and so must be in a uniform entry. */
#define SIGNAL_FROM(condition, start) do {                                \
        D c_ = (condition);                                             \
        DyFrame *f_ = find_handler(c_, (start));                        \
        if (f_) DY_TAIL_CALL(f_->b, c_, next_handler(c_, f_->prev), 0, 0, 2); \
        if (!default_handler)                                           \
            dy_error("a condition was signalled before the library was initialized", c_); \
        DY_TAIL_CALL(default_handler, c_, 0, 0, 0, 1);                  \
    } while (0)

static D next_handler_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    (void)a0; (void)a1; (void)a2; (void)a3;
    if (argc != 0) dy_wrong_argument_count(self, argc);
    DyFunction *n = DY_PTR(self);
    DyFrame *start = (DyFrame *)n->env[1];
    if (start && !in_chain(start, (uint64_t)DY_INT_VALUE(n->env[2])))
        dy_error("a next-handler was called outside its handler", 0);
    SIGNAL_FROM(n->env[0], start);
}

/* (%signal condition) */
PRIMITIVE(signal_condition) {
    need(self, argc, 1, 1);
    SIGNAL_FROM(a0, dy_thread->frames);
}

static D always_true_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {
    (void)self; (void)a0; (void)a1; (void)a2; (void)a3; (void)argc;
    return DY_TRUE;
}

static DyFunction always_true = { &dy_wrapper_method, always_true_entry, DY_FALSE,
                                  DY_FALSE };

/* (do-handlers funarg): funarg on each active handler's type, test,
 * function and description, the most recent first. */
PRIMITIVE(do_handlers) {
    need(self, argc, 1, 1);
    for (DyFrame *f = dy_thread->frames; f; f = f->prev)
        if (f->kind == DY_FRAME_HANDLER)
            DY_CALL(a0, f->a, f->c == DY_FALSE ? DY_OBJ(&always_true) : f->c, f->b,
                    f->d, 4);
    return DY_FALSE;
}

/* (%set-error-hooks error type-error default-handler): what the runtime
 * calls to signal the errors it finds, and default-handler. */
PRIMITIVE(set_error_hooks) {
    need(self, argc, 3, 3);
    error_hook = a0;
    type_error_hook = a1;
    default_handler = a2;
    return DY_FALSE;
}

/* ----------------------------------------------- The runtime's errors */

static _Noreturn void report(const char *kind, const char *message, D irritant) {
    fflush(stdout);
    fprintf(stderr, "Dylan %s: %s", kind, message);
    if (irritant) {
        fputs(": ", stderr);
        dy_debug_write(stderr, irritant);
    }
    fputc('\n', stderr);
    exit(1);
}

/* Signal a <simple-error> through the library, if it is ready and this is
 * not an error in signalling one; else report it and stop. */
_Noreturn void dy_error(const char *message, D irritant) {
    DyThread *t = dy_thread;
    if (error_hook && t && t->error_depth < 4) {
        t->error_depth++;
        size_t n = strlen(message);
        char *format = GC_MALLOC_ATOMIC(n + 5);
        memcpy(format, message, n);
        strcpy(format + n, irritant ? ": ~s" : "");
        DY_CALL(error_hook, dy_make_c_string(format),
                irritant ? dy_pair(irritant, DY_EMPTY) : DY_EMPTY, 0, 0, 2);
    }
    report("error", message, irritant);
}

void dy_check_type(D value, D type) {
    if (dy_specializer_matches(value, type)) return;
    DyThread *t = dy_thread;
    if (type_error_hook && t && t->error_depth < 4) {
        t->error_depth++;
        DY_CALL(type_error_hook, value, type, 0, 0, 2);
    }
    fflush(stdout);
    fputs("Dylan error: ", stderr);
    dy_debug_write(stderr, value);
    fputs(" is not an instance of ", stderr);
    dy_debug_write(stderr, type);
    fputc('\n', stderr);
    exit(1);
}

/* ---------------------------------------------------------- Messages */

static void display(FILE *out, D x) {
    if (dy_kind_p(x, DY_KIND_BYTE_STRING)) {
        DyByteString *s = DY_PTR(x);
        fwrite(s->data, 1, DY_INT_VALUE(s->size), out);
    } else if (DY_IS_CHAR(x) && DY_CHAR_CODE(x) < 256) {
        fputc((int)DY_CHAR_CODE(x), out);
    } else {
        dy_debug_write(out, x);
    }
}

/* (%format-message string arguments): STRING with ~a (display), ~s
 * (write), ~d, ~% and ~~ done, as the manual's examples use them;
 * arguments no directive uses follow, each after a space. */
PRIMITIVE(format_message) {
    need(self, argc, 2, 2);
    char *buffer = NULL;
    size_t size = 0;
    FILE *out = open_memstream(&buffer, &size);
    D args = a1;
    if (!dy_kind_p(a0, DY_KIND_BYTE_STRING)) {
        display(out, a0);
    } else {
        DyByteString *s = DY_PTR(a0);
        intptr_t n = DY_INT_VALUE(s->size);
        for (intptr_t i = 0; i < n; i++) {
            char c = s->data[i];
            if (c != '~' || i + 1 == n) { fputc(c, out); continue; }
            char d = s->data[++i];
            switch (d) {
            case 'a': case 'A': case 'd': case 'D': case 's': case 'S':
                if (dy_kind_p(args, DY_KIND_PAIR)) {
                    if (d == 's' || d == 'S') dy_debug_write(out, dy_head(args));
                    else display(out, dy_head(args));
                    args = dy_tail(args);
                }
                break;
            case '%': case '&': fputc('\n', out); break;
            case '~': fputc('~', out); break;
            default: fputc('~', out); fputc(d, out);
            }
        }
    }
    for (; dy_kind_p(args, DY_KIND_PAIR); args = dy_tail(args)) {
        fputc(' ', out);
        display(out, dy_head(args));
    }
    fclose(out);
    D string = dy_make_byte_string(buffer, size);
    free(buffer);
    return string;
}

/* The debugger of last resort: (%die message) reports and stops;
 * (%warn message) and (%break message) report and return #f. */
PRIMITIVE(die) {
    need(self, argc, 1, 1);
    fflush(stdout);
    fputs("Dylan error: ", stderr);
    display(stderr, a0);
    fputc('\n', stderr);
    exit(1);
}
PRIMITIVE(warn) {
    need(self, argc, 1, 1);
    fflush(stdout);
    fputs("Dylan warning: ", stderr);
    display(stderr, a0);
    fputc('\n', stderr);
    return DY_FALSE;
}
PRIMITIVE(break_message) {
    need(self, argc, 1, 1);
    fflush(stdout);
    fputs("Dylan break: ", stderr);
    display(stderr, a0);
    fputc('\n', stderr);
    return DY_FALSE;
}
