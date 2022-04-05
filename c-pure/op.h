#pragma once

#include "source_location.h"
#include "token.h"

#include <stddef.h>
#include <stdint.h>

#define OP_CODES_X \
    X(push) \
    X(plus) \
    X(minus) \
    X(eq) \
    X(if) \
    X(end) \
    X(dump)

typedef enum {
#define X(name) op_code_##name,
    OP_CODES_X
#undef X
} op_code_t;

typedef struct {
    op_code_t code;
    int64_t operand;
    token_t tok;
} op_t;

op_t push(int64_t operand, token_t tok);
op_t plus(token_t tok);
op_t dump(token_t tok);
op_t minus(token_t tok);
op_t eq(token_t tok);
op_t iff(token_t tok);
op_t end(token_t tok);
