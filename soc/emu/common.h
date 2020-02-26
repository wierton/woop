#ifndef EMU_COMMON_H
#define EMU_COMMON_H

#include <cstdio>
#include <cstdlib>
#include <cstring>

#define ESC_RED "\x1b[31m"
#define ESC_GREEN "\x1b[32m"
#define ESC_YELLOW "\x1b[33m"
#define ESC_BLUE "\x1b[34m"
#define ESC_MAGENTA "\x1b[35m"
#define ESC_CYAN "\x1b[36m"
#define ESC_RST "\x1b[0m"

// device
bool is_finished(void);
int get_exit_code(void);

#endif
