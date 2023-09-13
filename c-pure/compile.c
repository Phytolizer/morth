#include "compile.h"

#include "alloc_printf.h"
#include "c_emitter.h"
#include "generic_io.h"
#include "memory.h"
#include "nasm_emitter.h"
#include "run_command.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void compile_program_nasm(program_t program, generic_file_t f);
static void compile_program_c(program_t program, generic_file_t f);

bool parse_compile_target(const char* text, compile_target_t* out_target) {
    if (strcmp(text, "x86-64-linux-nasm") == 0 || strcmp(text, "x64-linux-nasm") == 0) {
        *out_target = COMPILE_TARGET_NASM;
        return true;
    }
    if (strcmp(text, "generic-c") == 0) {
        *out_target = COMPILE_TARGET_C;
        return true;
    }
    return false;
}

void compile_program(program_t program, compile_target_t target, const char* out_file_path) {
    generic_file_t f = generic_open(out_file_path);
    switch (target) {
        case COMPILE_TARGET_NASM:
            compile_program_nasm(program, f);
            break;
        case COMPILE_TARGET_C:
            compile_program_c(program, f);
            break;
    }
    generic_close(f);
}

void compile_program_native(
        program_t program, compile_target_t target, const char* out_file_basename) {
#ifdef _WIN32
#define OBJ_EXT ".obj"
#define NASM_FORMAT "win64"
#else // _WIN32
#define OBJ_EXT ".o"
#define NASM_FORMAT "elf64"
#endif // !_WIN32

#ifdef __clang__
#define CC_CMD "clang"
#define COFLAG(path) "-c", "-o", path
#define LINK_CMD "ld"
#define LINK_OUT_FMT "-o%s"
#else // __clang__
#define CC_CMD "cl"
#define COFLAG(path) "/c"
#define LINK_CMD "link.exe"
#define LINK_OUT_FMT "/out:%s"
#endif // !__clang__

    switch (target) {
        case COMPILE_TARGET_C: {
            char* c_path = NULL;
            alloc_sprintf(&c_path, "%s.c", out_file_basename);
            compile_program(program, target, c_path);
            char* obj_path = NULL;
            alloc_sprintf(&obj_path, "%s" OBJ_EXT, out_file_basename);
            RUN_COMMAND(CC_CMD, "-O2", COFLAG(obj_path), c_path);
            free(c_path);
            RUN_COMMAND(CC_CMD, "-o", out_file_basename, obj_path);
            free(obj_path);
        } break;
        case COMPILE_TARGET_NASM: {
            char* asm_path = NULL;
            alloc_sprintf(&asm_path, "%s.asm", out_file_basename);
            compile_program(program, target, asm_path);
            char* obj_path = NULL;
            alloc_sprintf(&obj_path, "%s" OBJ_EXT, out_file_basename);
            RUN_COMMAND("nasm", "-f", NASM_FORMAT, "-o", obj_path, asm_path);
            free(asm_path);
            char* link_args = NULL;
            alloc_sprintf(&link_args, LINK_OUT_FMT, out_file_basename);
            RUN_COMMAND(LINK_CMD, link_args, obj_path);
            free(obj_path);
            free(link_args);
        } break;
    }
}

static void compile_program_nasm(program_t program, generic_file_t f) {
    nasm_emitter_t em = nasm_emitter_open(f);
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

    nasm_emitter_close(em);
}

static void compile_program_c(program_t program, generic_file_t f) {
    c_emitter_t em = c_emitter_open(f);
#define EMIT(...) c_emitter_emit(&em, __VA_ARGS__)
#define LABL(...) c_emitter_emit_label(&em, __VA_ARGS__)
#define LINE() \
    do { \
        size_t temp_depth = em.indent_depth; \
        em.indent_depth = 0; \
        generic_write(em.f, "\n"); \
        em.indent_depth = temp_depth; \
    } while (false)

    EMIT("#include <inttypes.h>");
    EMIT("#include <stddef.h>");
    EMIT("#include <stdio.h>");
    EMIT("#include <stdint.h>");
    EMIT("#include <stdlib.h>");
    LINE();
    EMIT("void dump(intptr_t n) {");
    em.indent_depth += 1;
    EMIT("printf(\"%%\" PRIdPTR \"\\n\", n);");
    em.indent_depth -= 1;
    EMIT("}");
    LINE();
    EMIT("static uint8_t mem[%d];", MEM_CAPACITY);
    EMIT("static intptr_t stack[128];");
    EMIT("static size_t stack_top = 0;");
    EMIT("int main(void) {");
    em.indent_depth += 1;
    EMIT("intptr_t a, b, c, d;");

    for (size_t i = 0; i < program.length; i++) {
        op_t op = program.begin[i];
        LINE();
        LABL("addr_%zu", i);
        EMIT("// -- '%s' --", op.tok.text);
        switch (op.code) {
            case op_code_push:
                EMIT("stack[stack_top++] = %" PRId64 ";\n", op.operand);
                break;
            case op_code_plus:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a += b;");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_minus:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a -= b;");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_eq:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a = (a == b);");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_if:
                EMIT("a = stack[--stack_top];");
                EMIT("if (!a) {");
                em.indent_depth += 1;
                EMIT("goto addr_%zu;", op.operand);
                em.indent_depth -= 1;
                EMIT("}");
                break;
            case op_code_else:
                EMIT("goto addr_%zu;", op.operand);
                break;
            case op_code_end:
                EMIT("goto addr_%zu;", op.operand);
                break;
            case op_code_dump:
                EMIT("a = stack[--stack_top];");
                EMIT("dump(a);");
                break;
            case op_code_dup:
                EMIT("a = stack[--stack_top];");
                EMIT("stack[stack_top++] = a;");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_while:
                break;
            case op_code_do:
                EMIT("a = stack[--stack_top];");
                EMIT("if (!a) {");
                em.indent_depth += 1;
                EMIT("goto addr_%zu;", op.operand);
                em.indent_depth -= 1;
                EMIT("}");
                break;
            case op_code_gt:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a = (a > b);");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_lt:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a = (a < b);");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_mem:
                EMIT("stack[stack_top++] = (intptr_t)mem;");
                break;
            case op_code_load:
                EMIT("a = stack[--stack_top];");
                EMIT("b = *(const uint8_t*)a;");
                EMIT("stack[stack_top++] = b;");
                break;
            case op_code_store:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("*(uint8_t*)a = b;");
                break;
            case op_code_syscall1:
                assert(false && "unhandled syscall");
                break;
            case op_code_syscall3:
                EMIT("a = stack[--stack_top];  // syscall number");
                EMIT("b = stack[--stack_top];  // arg1");
                EMIT("c = stack[--stack_top];  // arg2");
                EMIT("d = stack[--stack_top];  // arg3");
                EMIT("if (a == 1) {");
                em.indent_depth += 1;
                EMIT("const char* s = (const char*)c;");
                EMIT("a = (intptr_t)fwrite(s, 1, d, stdout);");
                em.indent_depth -= 1;
                EMIT("} else {");
                em.indent_depth += 1;
                EMIT("abort();");
                em.indent_depth -= 1;
                EMIT("}");
                break;
            case op_code_dup2:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("stack[stack_top++] = a;");
                EMIT("stack[stack_top++] = b;");
                EMIT("stack[stack_top++] = a;");
                EMIT("stack[stack_top++] = b;");
                break;
            case op_code_drop:
                EMIT("--stack_top;");
                break;
            case op_code_shr:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a = (intptr_t)((uintptr_t)a >> (uintptr_t)b);");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_shl:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a = (intptr_t)((uintptr_t)a << (uintptr_t)b);");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_bor:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a = (intptr_t)((uintptr_t)a | (uintptr_t)b);");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_band:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("a = (intptr_t)((uintptr_t)a & (uintptr_t)b);");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_swap:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("stack[stack_top++] = b;");
                EMIT("stack[stack_top++] = a;");
                break;
            case op_code_over:
                EMIT("b = stack[--stack_top];");
                EMIT("a = stack[--stack_top];");
                EMIT("stack[stack_top++] = a;");
                EMIT("stack[stack_top++] = b;");
                EMIT("stack[stack_top++] = a;");
                break;
            default:
                assert(false && "unhandled opcode");
        }
    }

    LABL("addr_%zu", program.length);
    EMIT(";");
    em.indent_depth -= 1;
    EMIT("}");

    c_emitter_close(em);
}
