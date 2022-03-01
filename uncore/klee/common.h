#ifndef EMU_COMMON_H
#define EMU_COMMON_H

#include <cstdio>
#include <cstdlib>
#include <cstring>

extern "C" {
void klee_make_symbolic(void *, int, const char *);
void klee_assume(int);
}

#define ESC_RED "\x1b[31m"
#define ESC_GREEN "\x1b[32m"
#define ESC_YELLOW "\x1b[33m"
#define ESC_BLUE "\x1b[34m"
#define ESC_MAGENTA "\x1b[35m"
#define ESC_CYAN "\x1b[36m"
#define ESC_RST "\x1b[0m"

#ifdef DEBUG
#define dprintf(...) fprintf(stderr, __VA_ARGS__)
#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#else
#define dprintf(...)
#define eprintf(...)
#endif

#endif
