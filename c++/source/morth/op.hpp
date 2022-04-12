#pragma once

#include "morth/shorter_types.hpp"
#include "morth/token.hpp"
#include <magic_enum.hpp>

enum struct OpCode {
    PUSH,
    PLUS,
    MINUS,
    DUMP,
};

struct Op {
    OpCode code;
    Int64 value;
    Token tok;
};

Op Push(Int64 value, Token tok);
Op Plus(Token tok);
Op Minus(Token tok);
Op Dump(Token tok);
