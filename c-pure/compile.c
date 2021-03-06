#include "compile.h"

#include "memory.h"
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
#define LEFT(...) nasm_emitter_emit_left(&em, __VA_ARGS__)
#define LABL(...) nasm_emitter_emit_label(&em, __VA_ARGS__)
#define LINE() nasm_emitter_emit_left(&em, "")

    LEFT("section .text");
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
    LINE();
    EMIT("global _start");
    LABL("_start");

    for (size_t i = 0; i < program.length; i++) {
        op_t op = program.begin[i];
        LINE();
        LABL("addr_%zu", i);
        EMIT(";; -- '%s' --", op.tok.text);
        switch (op.code) {
            case op_code_push:
                EMIT("push %" PRId64, op.operand);
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
            case op_code_eq:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("cmp rax, rdx");
                EMIT("sete al");
                EMIT("movzx rax, al");
                EMIT("push rax");
                break;
            case op_code_if:
                EMIT("pop rax");
                EMIT("cmp rax, 0");
                EMIT("je addr_%zu", op.operand);
                break;
            case op_code_else:
                EMIT("jmp addr_%zu", op.operand);
                break;
            case op_code_end:
                EMIT("jmp addr_%zu", op.operand);
                break;
            case op_code_dump:
                EMIT("pop rdi");
                EMIT("call dump");
                break;
            case op_code_dup:
                EMIT("pop rax");
                EMIT("push rax");
                EMIT("push rax");
                break;
            case op_code_while:
                break;
            case op_code_do:
                EMIT("pop rax");
                EMIT("cmp rax, 0");
                EMIT("je addr_%zu", op.operand);
                break;
            case op_code_gt:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("cmp rax, rdx");
                EMIT("setg al");
                EMIT("movzx rax, al");
                EMIT("push rax");
                break;
            case op_code_lt:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("cmp rax, rdx");
                EMIT("setl al");
                EMIT("movzx rax, al");
                EMIT("push rax");
                break;
            case op_code_mem:
                EMIT("push mem");
                break;
            case op_code_load:
                EMIT("pop rax");
                EMIT("xor rbx, rbx");
                EMIT("mov bl, [rax]");
                EMIT("push rbx");
                break;
            case op_code_store:
                EMIT("pop rbx");
                EMIT("pop rax");
                EMIT("mov [rax], bl");
                break;
            case op_code_syscall1:
                EMIT("pop rax");
                EMIT("pop rdi");
                EMIT("syscall");
                break;
            case op_code_syscall3:
                EMIT("pop rax");
                EMIT("pop rdi");
                EMIT("pop rsi");
                EMIT("pop rdx");
                EMIT("syscall");
                break;
            case op_code_dup2:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("push rax");
                EMIT("push rdx");
                EMIT("push rax");
                EMIT("push rdx");
                break;
            case op_code_drop:
                EMIT("pop rax");
                break;
            case op_code_shr:
                EMIT("pop rcx");
                EMIT("pop rax");
                EMIT("shr rax, cl");
                EMIT("push rax");
                break;
            case op_code_shl:
                EMIT("pop rcx");
                EMIT("pop rax");
                EMIT("shl rax, cl");
                EMIT("push rax");
                break;
            case op_code_bor:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("or rax, rdx");
                EMIT("push rax");
                break;
            case op_code_band:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("and rax, rdx");
                EMIT("push rax");
                break;
            case op_code_swap:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("push rdx");
                EMIT("push rax");
                break;
            case op_code_over:
                EMIT("pop rdx");
                EMIT("pop rax");
                EMIT("push rax");
                EMIT("push rdx");
                EMIT("push rax");
                break;
            default:
                assert(false && "unhandled opcode");
        }
    }

    LINE();
    LABL("addr_%zu", program.length);
    EMIT("mov rax, 60");
    EMIT("mov rdi, 0");
    EMIT("syscall");
    LINE();
    LEFT("segment .bss");
    EMIT("mem: resb %d", MEM_CAPACITY);

#undef LABL
#undef LEFT
#undef LINE
#undef EMIT

    fclose(em.fp);
    close(raw_out);
}
