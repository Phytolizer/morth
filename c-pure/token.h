#pragma once

#include <stddef.h>

typedef struct {
    const char* file_path;
    const char* text;
    size_t row;
    size_t col;
} token_t;
