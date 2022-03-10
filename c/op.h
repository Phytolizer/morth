#ifndef MORTH_OP_H_
#define MORTH_OP_H_

#include <stdint.h>

#define OPS_X                                                                                      \
    X(push)                                                                                        \
    X(plus)                                                                                        \
    X(dump)

typedef enum {
#define X(x) op_code_##x,
    OPS_X
#undef X
        op_code_count
} op_code_t;

const char* op_code_as_cstr(op_code_t code);

typedef struct {
    op_code_t code;
    uint64_t value;
} op_t;

op_t op_push(uint64_t value);
op_t op_plus(void);
op_t op_dump(void);

#endif
