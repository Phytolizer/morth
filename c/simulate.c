#include "./simulate.h"

#include "./op.h"
#include "./stack.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

void simulate_program(program_t program) {
    stack_t stack = STACK_INIT;
    for (size_t ip = 0; ip < program.size; ip += 1) {
        switch (program.data[ip].code) {
            case op_code_push:
                stack_push(&stack, program.data[ip].value);
                break;
            case op_code_plus: {
                uint64_t b = stack_pop(&stack);
                uint64_t a = stack_pop(&stack);
                stack_push(&stack, a + b);
                break;
            }
            case op_code_minus: {
                uint64_t b = stack_pop(&stack);
                uint64_t a = stack_pop(&stack);
                stack_push(&stack, a - b);
                break;
            }
            case op_code_dump: {
                uint64_t value = stack_pop(&stack);
                printf("%" PRIu64 "\n", value);
                break;
            }
            case op_code_count:
            default:
                fprintf(stderr, "Encountered illegal opcode (%d)\n",
                        program.data[ip].code);
                exit(1);
        }
    }
    free(stack.data);
}
