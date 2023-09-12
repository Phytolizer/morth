#pragma once

#include "attr.h"
#include "generic_io.h"

#include <stdio.h>

typedef struct {
    generic_file_t f;
    size_t indent_depth;
} c_emitter_t;

c_emitter_t c_emitter_open(generic_file_t f);
void c_emitter_close(c_emitter_t em);
void c_emitter_emit_label(c_emitter_t* em, const char* format, ...) PRINTF_ATTR(2, 3);
void c_emitter_emit(c_emitter_t* em, const char* format, ...) PRINTF_ATTR(2, 3);
