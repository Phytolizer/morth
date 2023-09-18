#pragma once

#include "attr.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#endif // _WIN32

typedef struct {
#ifdef _WIN32
    HANDLE fh;
#else // _WIN32
    int fd;
#endif // !_WIN32
} generic_file_t;

generic_file_t generic_open(char const* path);
void generic_close(generic_file_t f);
void generic_write(generic_file_t f, char const* text);
void generic_printf(generic_file_t f, char const* format, ...) PRINTF_ATTR(2, 3);
