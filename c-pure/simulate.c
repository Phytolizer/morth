#include "simulate.h"

#include "stack.h"

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

void simulate_program(program_t program) {
    stack_t stack = STACK_INIT;
    for (size_t i = 0; i < program.length; i++) {
        op_t op = program.begin[i];
        switch (op.code) {
            case op_code_push:
                stack_push(&stack, op.operand);
                break;
            case op_code_plus: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a + b);
            } break;
            case op_code_minus: {
                int64_t b = stack_pop(&stack);
                int64_t a = stack_pop(&stack);
                stack_push(&stack, a - b);
            } break;
            case op_code_dump: {
                int64_t value = stack_pop(&stack);
                printf("%" PRId64 "\n", value);
            } break;
            default:
                assert(false && "unhandled opcode");
        }
    }
    STACK_FREE(stack);
}
