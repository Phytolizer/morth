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
    X(else) \
    X(end) \
    X(dump) \
    X(dup) \
    X(while) \
    X(do) \
    X(gt) \
    X(lt) \
    X(mem) \
    X(load) \
    X(store) \
    X(syscall1) \
    X(syscall3) \
    X(dup2) \
    X(drop) \
    X(shr) \
    X(shl) \
    X(bor) \
    X(band) \
    X(swap) \
    X(over)

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
op_t elze(token_t tok);
op_t end(token_t tok);
op_t dupp(token_t tok);
op_t wile(token_t tok);
op_t doo(token_t tok);
op_t gt(token_t tok);
op_t lt(token_t tok);
op_t mem(token_t tok);
op_t load(token_t tok);
op_t store(token_t tok);
op_t syscall1(token_t tok);
op_t syscall3(token_t tok);
op_t dupp2(token_t tok);
op_t drop(token_t tok);
op_t shr(token_t tok);
op_t shl(token_t tok);
op_t bor(token_t tok);
op_t band(token_t tok);
op_t swap(token_t tok);
op_t over(token_t tok);
