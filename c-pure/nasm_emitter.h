#pragma once

#include <stdio.h>

typedef struct {
    FILE* fp;
} nasm_emitter_t;

nasm_emitter_t nasm_emitter_from_fd(int fd);
void nasm_emitter_emit_label(nasm_emitter_t* emitter, const char* format, ...);
void nasm_emitter_emit(nasm_emitter_t* emitter, const char* format, ...);
