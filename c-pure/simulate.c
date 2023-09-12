#include "simulate.h"

#include "memory.h"
#include "stack.h"

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

void simulate_program(program_t program) {
    stack_t stack = STACK_INIT;
    uint8_t mem[MEM_CAPACITY];
    for (size_t ip = 0; ip < program.length;) {
        op_t op = program.begin[ip];
        switch (op.code) {
            case op_code_push:
                stack_push(&stack, op.operand);
                ip++;
                break;
            case op_code_plus: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a + b);
                ip++;
            } break;
            case op_code_minus: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a - b);
                ip++;
            } break;
            case op_code_eq: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a == b);
                ip++;
            } break;
            case op_code_dump: {
                int64_t value = stack_pop(&stack);
                printf("%" PRId64 "\n", value);
                ip++;
            } break;
            case op_code_dup: {
                int64_t value = stack_pop(&stack);
                stack_push(&stack, value);
                stack_push(&stack, value);
                ip++;
            } break;
            case op_code_gt: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a > b);
                ip++;
            } break;
            case op_code_lt: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a < b);
                ip++;
            } break;
            case op_code_if: {
                int64_t condition = stack_pop(&stack);
                if (condition) {
                    ip++;
                } else {
                    ip = op.operand;
                }
            } break;
            case op_code_else:
                ip = op.operand;
                break;
            case op_code_while:
                ip++;
                break;
            case op_code_do: {
                int64_t condition = stack_pop(&stack);
                if (condition) {
                    ip++;
                } else {
                    ip = op.operand;
                }
            } break;
            case op_code_end:
                ip = op.operand;
                break;
            case op_code_mem:
                stack_push(&stack, 0);
                ip++;
                break;
            case op_code_load: {
                int64_t address = stack_pop(&stack);
                stack_push(&stack, mem[address]);
                ip++;
            } break;
            case op_code_store: {
                int64_t value = stack_pop(&stack);
                int64_t address = stack_pop(&stack);
                mem[address] = (uint8_t)value;
                ip++;
            } break;
            case op_code_syscall1:
                assert(false && "unhandled syscall");
            case op_code_syscall3: {
                int64_t syscall_number = stack_pop(&stack);
                int64_t arg1 = stack_pop(&stack);
                int64_t arg2 = stack_pop(&stack);
                int64_t arg3 = stack_pop(&stack);
                if (syscall_number == 1) {
                    int64_t fd = arg1;
                    int64_t buf = arg2;
                    int64_t count = arg3;
                    uint8_t* s = &mem[buf];
                    FILE* fp = NULL;
                    switch (fd) {
                        case 1:
                            fp = stdout;
                            break;
                        case 2:
                            fp = stderr;
                            break;
                        default:
                            assert(false && "invalid fd");
                    }
                    fwrite(s, 1, count, fp);
                } else {
                    assert(false && "unhandled syscall");
                }
                ip++;
            } break;
            case op_code_dup2: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a);
                stack_push(&stack, b);
                stack_push(&stack, a);
                stack_push(&stack, b);
                ip++;
            } break;
            case op_code_drop:
                stack_pop(&stack);
                ip++;
                break;
            case op_code_shr: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a >> b);
                ip++;
            } break;
            case op_code_shl: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a << b);
                ip++;
            } break;
            case op_code_bor: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a | b);
                ip++;
            } break;
            case op_code_band: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a & b);
                ip++;
            } break;
            case op_code_swap: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, b);
                stack_push(&stack, a);
                ip++;
            } break;
            case op_code_over: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a);
                stack_push(&stack, b);
                stack_push(&stack, a);
                ip++;
            } break;
            default:
                assert(false && "unhandled opcode");
        }
    }
    STACK_FREE(stack);
}
