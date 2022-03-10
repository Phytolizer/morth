#include "./op.h"

static const char* const op_code_strings[] = {
#define X(x) #x,
    OPS_X
#undef X
};

const char* op_code_as_cstr(op_code_t code) {
    if (code >= 0 && code < op_code_count) {
        return op_code_strings[code];
    }
    return "<ILLEGAL OPCODE>";
}

op_t op_push(uint64_t value) {
    return (op_t){
        .code = op_code_push,
        .value = value,
    };
}

op_t op_plus(void) {
    return (op_t){
        .code = op_code_plus,
    };
}

op_t op_dump(void) {
    return (op_t){
        .code = op_code_dump,
    };
}
