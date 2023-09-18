#pragma once

#include "attr.h"
#include "generic_io.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#endif // _WIN32

typedef struct {
    generic_file_t f;
} nasm_emitter_t;

nasm_emitter_t nasm_emitter_open(generic_file_t f);
void nasm_emitter_close(nasm_emitter_t em);
void nasm_emitter_emit_label(nasm_emitter_t* em, char const* format, ...);
void nasm_emitter_emit_left(nasm_emitter_t* em, char const* format, ...);
void nasm_emitter_emit(nasm_emitter_t* em, char const* format, ...);
