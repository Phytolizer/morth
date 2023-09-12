#pragma once

#include "program.h"

typedef enum {
    COMPILE_TARGET_C,
    COMPILE_TARGET_NASM,
} compile_target_t;

/// Set `*out_target` on match.
///
/// return false if it matches none
bool parse_compile_target(const char* text, compile_target_t* out_target);

void compile_program(program_t program, compile_target_t target, const char* out_file_path);
void compile_program_native(
        program_t program, compile_target_t target, const char* out_file_basename);
