#include "morth/op.hpp"

Op Push(Int64 value, Token tok) {
    Op op;
    op.code = OpCode::PUSH;
    op.value = value;
    op.tok = tok;
    return op;
}

Op Plus(Token tok) {
    Op op;
    op.code = OpCode::PLUS;
    op.tok = tok;
    return op;
}

Op Minus(Token tok) {
    Op op;
    op.code = OpCode::MINUS;
    op.tok = tok;
    return op;
}

Op Dump(Token tok) {
    Op op;
    op.code = OpCode::DUMP;
    op.tok = tok;
    return op;
}
