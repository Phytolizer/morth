#include "simulate.h"

#include "stack.h"

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

void simulate_program(program_t program) {
    stack_t stack = STACK_INIT;
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
            case op_code_end:
                ip++;
                break;
            default:
                assert(false && "unhandled opcode");
        }
    }
    STACK_FREE(stack);
}
