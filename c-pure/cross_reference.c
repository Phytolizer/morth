#include "cross_reference.h"

#include "buffer.h"

#include <assert.h>
#include <stdio.h>

typedef BUFFER_TYPE(size_t) size_stack_t;

static void size_stack_push(size_stack_t* stack, size_t value);
static size_t size_stack_pop(size_stack_t* stack);
static void die_unbalanced_token(token_t token);

void cross_reference_blocks(program_t program) {
    size_stack_t stack = BUFFER_INIT;
    for (size_t ip = 0; ip < program.length; ip++) {
        op_t op = program.begin[ip];
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
            case op_code_gt:
                break;
            case op_code_lt:
                break;
            case op_code_dup:
                break;
            case op_code_mem:
                break;
            case op_code_load:
                break;
            case op_code_store:
                break;
            case op_code_syscall1:
                break;
            case op_code_syscall3:
                break;
            case op_code_dup2:
                break;
            case op_code_drop:
                break;
            case op_code_shr:
                break;
            case op_code_shl:
                break;
            case op_code_bor:
                break;
            case op_code_band:
                break;
            case op_code_swap:
                break;
            case op_code_over:
                break;
            case op_code_if:
                size_stack_push(&stack, ip);
                break;
            case op_code_else: {
                size_t if_ip = size_stack_pop(&stack);
                program.begin[if_ip].operand = ip + 1;
                size_stack_push(&stack, ip);
            } break;
            case op_code_while:
                size_stack_push(&stack, ip);
                break;
            case op_code_do: {
                size_t while_ip = size_stack_pop(&stack);
                program.begin[ip].operand = while_ip;
                size_stack_push(&stack, ip);
            } break;
            case op_code_end: {
                if (stack.length == 0) {
                    die_unbalanced_token(op.tok);
                }
                size_t block_ip = size_stack_pop(&stack);
                switch (program.begin[block_ip].code) {
                    case op_code_if:
                    case op_code_else:
                        program.begin[block_ip].operand = ip;
                        program.begin[ip].operand = ip + 1;
                        break;
                    case op_code_do: {
                        size_t while_ip = (size_t)program.begin[block_ip].operand;
                        program.begin[block_ip].operand = ip + 1;
                        program.begin[ip].operand = while_ip;
                    } break;
                    default:
                        die_unbalanced_token(program.begin[block_ip].tok);
                }
            } break;
            default:
                assert(false && "unhandled opcode");
        }
    }
    if (stack.length > 0) {
        size_t ip = size_stack_pop(&stack);
        op_t op = program.begin[ip];
        die_unbalanced_token(op.tok);
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

static void die_unbalanced_token(token_t token) {
    fprintf(stderr, "%s:%zu:%zu: error: unbalanced '%s'\n", token.loc.file_path, token.loc.row,
            token.loc.col, token.text);
    exit(EXIT_FAILURE);
}
