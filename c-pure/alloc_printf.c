#include "alloc_printf.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

int alloc_sprintf(char** out, char const* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int n = vsnprintf(NULL, 0, fmt, args);
    va_end(args);
    if (n < 0)
        return n;

    *out = malloc(n + 1);
    if (!out)
        return -1;

    va_start(args, fmt);
    n = vsnprintf(*out, n + 1, fmt, args);
    va_end(args);
    return n;
}

int alloc_vsprintf(char** out, char const* fmt, va_list args) {
    va_list args2;
    va_copy(args2, args);
    int n = vsnprintf(NULL, 0, fmt, args2);
    va_end(args2);
    if (n < 0)
        return n;

    *out = malloc(n + 1);
    if (!out)
        return -1;

    return vsnprintf(*out, n + 1, fmt, args);
}
