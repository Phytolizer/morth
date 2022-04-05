#pragma once

#include <stddef.h>

typedef struct {
    const char* file_path;
    size_t row;
    size_t col;
} source_location_t;
