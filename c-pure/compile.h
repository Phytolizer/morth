#pragma once

#include "program.h"

typedef enum {
    COMPILE_TARGET_C,
    COMPILE_TARGET_NASM,
} compile_target_t;

/// Set `*out_target` on match.
///
/// return false if it matches none
bool parse_compile_target(char const* text, compile_target_t* out_target);

char* compute_output_path(char const* input_file_path, char const* output_file_path);

void compile_program(program_t program, compile_target_t target, char const* out_file_path);
void compile_program_native(
        program_t program, compile_target_t target, char const* out_file_basename);
