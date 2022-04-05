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
