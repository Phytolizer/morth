#include "./compile.h"

#include "./op.h"

#include <inttypes.h>
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
    fprintf(output, "extern dump\n");
    fprintf(output, "global _start\n");
    fprintf(output, "_start:\n");

    for (size_t ip = 0; ip < program.size; ip += 1) {
        femitf(output, ";; -- %s --", op_code_as_cstr(program.data[ip].code));
        switch (program.data[ip].code) {
            case op_code_push:
                femitf(output, "push %" PRIu64, program.data[ip].value);
                break;
            case op_code_plus:
                femitf(output, "pop rbx");
                femitf(output, "pop rax");
                femitf(output, "add rax, rbx");
                femitf(output, "push rax");
                break;
            case op_code_minus:
                femitf(output, "pop rbx");
                femitf(output, "pop rax");
                femitf(output, "sub rax, rbx");
                femitf(output, "push rax");
                break;
            case op_code_dump:
                femitf(output, "pop rdi");
                femitf(output, "call dump");
                break;
            case op_code_count:
            default:
                fprintf(stderr, "illegal opcode %d\n", program.data[ip].code);
                exit(1);
        }
    }

    femitf(output, "mov rax, 60");
    femitf(output, "mov rdi, 0");
    femitf(output, "syscall");

    fclose(output);
}
