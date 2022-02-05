#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OP_CODES_X                                                                                                     \
    X(PUSH)                                                                                                            \
    X(PLUS)                                                                                                            \
    X(MINUS)                                                                                                           \
    X(DUMP)                                                                                                            \
    X(HALT)

enum op_code
{
#define X(x) OP_##x,
    OP_CODES_X
#undef X
        OPS_COUNT,
};

const char* const OP_STRINGS[] = {
#define X(x) #x,
    OP_CODES_X
#undef X
};

struct op
{
    enum op_code code;
    union {
        uint64_t push_op;
    } as;
};

struct op push(uint64_t x)
{
    return (struct op){
        .code = OP_PUSH,
        .as = {.push_op = x},
    };
}

struct op plus(void)
{
    return (struct op){
        .code = OP_PLUS,
    };
}

struct op dump(void)
{
    return (struct op){
        .code = OP_DUMP,
    };
}

struct op halt(void)
{
    return (struct op){
        .code = OP_HALT,
    };
}

struct stack
{
    uint64_t* data;
    size_t length;
    size_t capacity;
};

void stack_push(struct stack* stack, uint64_t x)
{
    if (stack->length + 1 > stack->capacity)
    {
        size_t old_capacity = stack->capacity;
        stack->capacity = stack->capacity * 2 + 1;
        uint64_t* new_data = calloc(sizeof(uint64_t), stack->capacity);
        memcpy(new_data, stack->data, old_capacity * sizeof(uint64_t));
        free(stack->data);
        stack->data = new_data;
    }

    stack->data[stack->length] = x;
    stack->length += 1;
}

uint64_t stack_pop(struct stack* stack)
{
    if (stack->length == 0)
    {
        fputs("fatal: stack underflow\n", stderr);
        exit(1);
    }

    stack->length -= 1;
    return stack->data[stack->length];
}

void simulate_program(struct op* program)
{
    static_assert(OPS_COUNT == 5, "simulate_program is out of sync");
    struct stack stack = {0};
    for (size_t ip = 0; program[ip].code != OP_HALT;)
    {
        switch (program[ip].code)
        {
            case OP_PUSH:
                stack_push(&stack, program[ip].as.push_op);
                ip += 1;
                break;
            case OP_PLUS: {
                uint64_t y = stack_pop(&stack);
                uint64_t x = stack_pop(&stack);
                stack_push(&stack, x + y);
                ip += 1;
                break;
            }
            case OP_DUMP: {
                uint64_t x = stack_pop(&stack);
                printf("%" PRIu64 "\n", x);
                ip += 1;
                break;
            }
            case OP_HALT:
                assert(false && "unreachable");
            case OPS_COUNT:
            default:
                fputs("fatal: corrupt opcode encountered\n", stderr);
                exit(1);
        }
    }
}

void compile_program(struct op* program, FILE* stream)
{
    static_assert(OPS_COUNT == 5, "compile_program is out of sync");
    for (size_t ip = 0; program[ip].code != OP_HALT; ip += 1)
    {
        fprintf(stream, ";; -- %s --\n", OP_STRINGS[program[ip].code]);
        switch (program[ip].code)
        {
            case OP_PUSH:
                fputs("push rax\n", stream);
                break;
            case OP_PLUS:
                fputs("pop rbx\n", stream);
                fputs("pop rax\n", stream);
                fputs("add rax, rbx\n", stream);
                fputs("push rax\n", stream);
                break;
        }
    }
}

void usage(const char* program_name)
{
    printf("usage: %s <SUBCOMMAND> [ARGS]\n", program_name);
    puts("  SUBCOMMANDS:");
    puts("    sim      Simulate the program");
    puts("    com      Compile the program");
}

int main(int argc, char** argv)
{
    struct op program[] = {
        push(34),
        push(35),
        plus(),
        dump(),
        halt(),
    };

    if (argc < 2)
    {
        usage(argv[0]);
        fputs("ERROR: no subcommand is provided\n", stderr);
        exit(1);
    }

    if (strcmp(argv[1], "sim") == 0)
    {
        simulate_program(program);
    }
    else if (strcmp(argv[1], "com") == 0)
    {
        FILE* out = fopen("output.asm", "w");
        if (out == NULL)
        {
            fputs("fatal: could not open output.asm\n", stderr);
            exit(1);
        }
        fputs("segment .text\n", out);
        fputs("global _start\n", out);
        fputs("_start:\n", out);
        fputs("mov rax, 60\n", out);
        fputs("mov rdi, 0\n", out);
        fputs("syscall\n", out);
        compile_program(program, out);
        fclose(out);
    }
    else
    {
        usage(argv[0]);
        fprintf(stderr, "ERROR: unknown subcommand '%s'\n", argv[1]);
        exit(1);
    }
}
