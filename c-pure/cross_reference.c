#include "cross_reference.h"

#include "buffer.h"

#include <assert.h>

typedef BUFFER_TYPE(size_t) size_stack_t;

static void size_stack_push(size_stack_t* stack, size_t value);
static size_t size_stack_pop(size_stack_t* stack);

void cross_reference_blocks(program_t program) {
    size_stack_t stack = BUFFER_INIT;
    for (size_t i = 0; i < program.length; i++) {
        op_t op = program.begin[i];
        switch (op.code) {
            case op_code_push:
                break;
            case op_code_plus:
                break;
            case op_code_minus:
                break;
            case op_code_eq:
                break;
            case op_code_dump:
                break;
            case op_code_if:
                size_stack_push(&stack, i);
                break;
            case op_code_end: {
                size_t if_ip = size_stack_pop(&stack);
                program.begin[if_ip].operand = i;
            } break;
            default:
                assert(false && "unhandled opcode");
        }
    }

    BUFFER_FREE(stack);
}

static void size_stack_push(size_stack_t* stack, size_t value) {
    BUFFER_EXPAND(stack);
    stack->data[stack->length] = value;
    stack->length++;
}

static size_t size_stack_pop(size_stack_t* stack) {
    assert(stack->length > 0);
    stack->length--;
    return stack->data[stack->length];
}
