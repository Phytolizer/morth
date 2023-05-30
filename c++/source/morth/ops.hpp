#pragma once

#include "types.hpp"

#include <magic_enum.hpp>

namespace morth {

struct Op {
    enum class Code {
        PUSH,
        PLUS,
        MINUS,
        DUMP,
    };

    Code code;
    U64 arg;

    static constexpr Op Push(U64 arg) {
        return {Code::PUSH, arg};
    }

    static constexpr Op Plus() {
        return {Code::PLUS, 0};
    }

    static constexpr Op Minus() {
        return {Code::MINUS, 0};
    }

    static constexpr Op Dump() {
        return {Code::DUMP, 0};
    }
};

} // namespace morth
