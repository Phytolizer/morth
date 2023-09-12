#pragma once

#include "program.h"

typedef enum {
    COMPILE_TARGET_C,
    COMPILE_TARGET_NASM,
} compile_target_t;

void compile_program(program_t program, compile_target_t target, const char* out_file_path);
