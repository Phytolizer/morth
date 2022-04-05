#include "op.h"

op_t push(int64_t operand) {
    op_t result;
    result.code = op_code_push;
    result.operand = operand;
    return result;
}

op_t plus(void) {
    op_t result;
    result.code = op_code_plus;
    return result;
}

op_t dump(void) {
    op_t result;
    result.code = op_code_dump;
    return result;
}

op_t minus(void) {
    op_t result;
    result.code = op_code_minus;
    return result;
}

op_t eq(void) {
    op_t result;
    result.code = op_code_eq;
    return result;
}
