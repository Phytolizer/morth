#include "./op.h"
#include "./stack.h"

#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static void simulate_program(op_t* program, size_t program_size) {
    stack_t stack = STACK_INIT;
    for (size_t ip = 0; ip < program_size; ip += 1) {
        switch (program[ip].code) {
            case op_code_push:
                stack_push(&stack, program[ip].value);
                break;
            case op_code_plus: {
                uint64_t b = stack_pop(&stack);
                uint64_t a = stack_pop(&stack);
                stack_push(&stack, a + b);
                break;
            }
            case op_code_dump: {
                uint64_t value = stack_pop(&stack);
                printf("%" PRIu64 "\n", value);
                break;
            }
            case op_code_count:
            default:
                fprintf(stderr, "Encountered illegal opcode (%d)\n", program[ip].code);
                exit(1);
        }
    }
    free(stack.data);
}

int main(void) {
    op_t program[4];
    program[0] = op_push(34);
    program[1] = op_push(35);
    program[2] = op_plus();
    program[3] = op_dump();

    simulate_program(program, sizeof program / sizeof(op_t));

    return 0;
}
