#include <errno.h>
#include <inttypes.h>
#include <op_code.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define STRERROR_R(error, buf, size) strerror_s(buf, size, error)
#else
#define STRERROR_R strerror_r
#endif

#define ERRBUF_SIZE 64

#define TRY_T(T)                                                               \
    struct {                                                                   \
        T value;                                                               \
        int error;                                                             \
    }

typedef struct {
    op_code_t code;
    uint64_t value;
} op_t;

static op_t push(uint64_t value) {
    return (op_t){.code = op_code_push, .value = value};
}

static op_t plus(void) {
    return (op_t){.code = op_code_plus};
}

static op_t dump(void) {
    return (op_t){.code = op_code_dump};
}

typedef struct {
    op_t* data;
    size_t length;
} program_t;

typedef struct {
    uint64_t* data;
    size_t length;
    size_t capacity;
} simulation_stack_t;

int simulation_stack_push(simulation_stack_t* stack, uint64_t value) {
    if (stack->length == stack->capacity) {
        stack->capacity = stack->capacity * 2 + 1;
        uint64_t* new_data =
            realloc(stack->data, sizeof(uint64_t) * stack->capacity);
        if (new_data == NULL) {
            free(stack->data);
            stack->data = NULL;
            stack->length = 0;
            stack->capacity = 0;
            return ENOMEM;
        }
        stack->data = new_data;
    }
    stack->data[stack->length] = value;
    stack->length += 1;
    return 0;
}

typedef TRY_T(uint64_t) try_uint64_t;

#define ERR_STACK_UNDERFLOW 0x8000
#define ERR_ILLEGAL_OPCODE 0x8001

static void print_error(int error) {
    switch (error) {
        case ERR_STACK_UNDERFLOW:
            fprintf(stderr, "stack underflow\n");
            break;
        default: {
            char errbuf[ERRBUF_SIZE];
            STRERROR_R(error, errbuf, sizeof errbuf);
            fprintf(stderr, "%s\n", errbuf);
        } break;
    }
}

static try_uint64_t simulation_stack_pop(simulation_stack_t* stack) {
    if (stack->length == 0) {
        return (try_uint64_t){.error = ERR_STACK_UNDERFLOW};
    }
    stack->length -= 1;
    return (try_uint64_t){.value = stack->data[stack->length]};
}

static int simulate_program(program_t program) {
    simulation_stack_t stack = {0};

    for (size_t i = 0; i < program.length; i += 1) {
        switch (program.data[i].code) {
            case op_code_push:
                simulation_stack_push(&stack, program.data[i].value);
                break;
            case op_code_plus: {
                try_uint64_t b = simulation_stack_pop(&stack);
                if (b.error != 0) {
                    free(stack.data);
                    return b.error;
                }
                try_uint64_t a = simulation_stack_pop(&stack);
                if (a.error != 0) {
                    free(stack.data);
                    return a.error;
                }
                simulation_stack_push(&stack, a.value + b.value);
            } break;
            case op_code_dump: {
                try_uint64_t value = simulation_stack_pop(&stack);
                if (value.error != 0) {
                    free(stack.data);
                    return value.error;
                }
                printf("%" PRIu64 "\n", value.value);
            } break;
            case count_op_code:
            default:
                free(stack.data);
                return ERR_ILLEGAL_OPCODE;
        }
    }

    free(stack.data);
    return 0;
}

int main(void) {
    op_t program[4];
    program[0] = push(34);
    program[1] = push(35);
    program[2] = plus();
    program[3] = dump();

    int error = simulate_program((program_t){
        .data = program,
        .length = sizeof program / sizeof(op_t),
    });
    if (error != 0) {
        print_error(error);
        return 1;
    }
}
