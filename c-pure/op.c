#include "op.h"

op_t push(int64_t operand, token_t tok) {
    op_t result;
    result.code = op_code_push;
    result.operand = operand;
    result.tok = tok;
    return result;
}

op_t plus(token_t tok) {
    op_t result;
    result.code = op_code_plus;
    result.tok = tok;
    return result;
}

op_t dump(token_t tok) {
    op_t result;
    result.code = op_code_dump;
    result.tok = tok;
    return result;
}

op_t minus(token_t tok) {
    op_t result;
    result.code = op_code_minus;
    result.tok = tok;
    return result;
}

op_t eq(token_t tok) {
    op_t result;
    result.code = op_code_eq;
    result.tok = tok;
    return result;
}

op_t iff(token_t tok) {
    op_t result;
    result.code = op_code_if;
    result.tok = tok;
    return result;
}

op_t elze(token_t tok) {
    op_t result;
    result.code = op_code_else;
    result.tok = tok;
    return result;
}

op_t end(token_t tok) {
    op_t result;
    result.code = op_code_end;
    result.tok = tok;
    return result;
}

op_t dupp(token_t tok) {
    op_t result;
    result.code = op_code_dup;
    result.tok = tok;
    return result;
}

op_t wile(token_t tok) {
    op_t result;
    result.code = op_code_while;
    result.tok = tok;
    return result;
}

op_t doo(token_t tok) {
    op_t result;
    result.code = op_code_do;
    result.tok = tok;
    return result;
}

op_t gt(token_t tok) {
    op_t result;
    result.code = op_code_gt;
    result.tok = tok;
    return result;
}

op_t lt(token_t tok) {
    op_t result;
    result.code = op_code_lt;
    result.tok = tok;
    return result;
}

op_t mem(token_t tok) {
    op_t result;
    result.code = op_code_mem;
    result.tok = tok;
    return result;
}

op_t load(token_t tok) {
    op_t result;
    result.code = op_code_load;
    result.tok = tok;
    return result;
}

op_t store(token_t tok) {
    op_t result;
    result.code = op_code_store;
    result.tok = tok;
    return result;
}

op_t syscall1(token_t tok) {
    op_t result;
    result.code = op_code_syscall1;
    result.tok = tok;
    return result;
}

op_t syscall3(token_t tok) {
    op_t result;
    result.code = op_code_syscall3;
    result.tok = tok;
    return result;
}

op_t dupp2(token_t tok) {
    op_t result;
    result.code = op_code_dup2;
    result.tok = tok;
    return result;
}
