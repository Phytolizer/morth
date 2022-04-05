#pragma once

#include <stdint.h>

#define OP_CODES_X \
    X(push) \
    X(plus) \
    X(dump)

typedef enum {
#define X(name) op_code_##name,
    OP_CODES_X
#undef X
} op_code_t;

typedef struct {
    op_code_t code;
    int64_t operand;
} op_t;

op_t push(int64_t operand);
op_t plus(void);
op_t dump(void);
