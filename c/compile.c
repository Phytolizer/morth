#include "./compile.h"

#include "./op.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

static void femitf(FILE* stream, const char* format, ...) {
    fprintf(stream, "    ");
    va_list args;
    va_start(args, format);
    vfprintf(stream, format, args);
    va_end(args);
    fprintf(stream, "\n");
}

void compile_program(program_t program, const char* output_name) {
    FILE* output = fopen(output_name, "w");
    if (output == NULL) {
        fprintf(stderr, "Failed opening output file.\n");
        exit(1);
    }

    fprintf(output, "segment .text\n");
    fprintf(output, "global _start\n");
    fprintf(output, "_start:\n");

    for (size_t ip = 0; ip < program.size; ip += 1) {
    }

    femitf(output, "mov rax, 60");
    femitf(output, "mov rdi, 0");
    femitf(output, "syscall");

    fclose(output);
}
