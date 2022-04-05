#include "compile.h"

#include "nasm_emitter.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void compile_program(program_t program, const char* out_file_path) {
    int raw_out = open(
            out_file_path, O_WRONLY | O_CREAT | O_TRUNC, S_IWUSR | S_IRUSR | S_IRGRP | S_IROTH);
    if (raw_out < 0) {
        perror("open");
        exit(EXIT_FAILURE);
    }

    nasm_emitter_t em = nasm_emitter_from_fd(raw_out);

#define EMIT(...) nasm_emitter_emit(&em, __VA_ARGS__)
#define LABL(...) nasm_emitter_emit_label(&em, __VA_ARGS__)

    EMIT("section .text");

    LABL("dump");
    EMIT("sub rsp, 40");
    EMIT("lea rsi, [rsp + 31]");
    EMIT("mov byte [rsp + 31], 10");
    EMIT("mov ecx, 1");
    EMIT("mov r8, -3689348814741910323");
    LABL(".LBB0_1");
    EMIT("mov rax, rdi");
    EMIT("mul r8");
    EMIT("shr rdx, 3");
    EMIT("lea eax, [rdx + rdx]");
    EMIT("lea r9d, [rax + 4*rax]");
    EMIT("mov eax, edi");
    EMIT("sub eax, r9d");
    EMIT("or al, 48");
    EMIT("mov byte [rsi - 1], al");
    EMIT("add rsi, -1");
    EMIT("add rcx, 1");
    EMIT("cmp rdi, 9");
    EMIT("mov rdi, rdx");
    EMIT("ja .LBB0_1");
    EMIT("mov edi, 1");
    EMIT("mov rdx, rcx");
    EMIT("mov rax, 1");
    EMIT("syscall");
    EMIT("add rsp, 40");
    EMIT("ret");

    EMIT("global _start");
    LABL("_start");

    for (op_t* op = program.begin; op != program.end; op++) {
        switch (op->code) {
            case op_code_push:
                EMIT("push %" PRId64, op->operand);
                break;
            case op_code_plus:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("add rax, rdx");
                EMIT("push rax");
                break;
            case op_code_minus:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("sub rax, rdx");
                EMIT("push rax");
                break;
            case op_code_dump:
                EMIT("pop rdi");
                EMIT("call dump");
                break;
            default:
                assert(false && "unhandled opcode");
        }
    }

    EMIT("mov rax, 60");
    EMIT("mov rdi, 0");
    EMIT("syscall");

#undef LABL
#undef EMIT

    fclose(em.fp);
    close(raw_out);
}
