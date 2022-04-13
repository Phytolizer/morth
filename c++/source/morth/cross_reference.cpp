#include "morth/cross_reference.hpp"

#include "morth/macros.hpp"
#include "morth/shorter_types.hpp"

#include <fmt/format.h>
#include <stack>
#include <stdexcept>

MORTH_FILE_LOCAL void dieUnbalancedToken(Token tok) {
    fmt::print(stderr, "{}:{}:{}: Unbalanced '{}'\n", tok.location.filePath, tok.location.row,
               tok.location.column, tok.text);
    throw std::runtime_error{"Unbalanced token"};
}

void CrossReferenceBlocks(Program* program) {
    std::stack<Size> stack;
    for (Size ip = 0; ip < program->size(); ip++) {
        const auto& instruction = (*program)[ip];
        switch (instruction.code) {
            case OpCode::PUSH:
            case OpCode::PLUS:
            case OpCode::MINUS:
            case OpCode::DUMP:
            default:
                break;
        }
    }
    if (!stack.empty()) {
        Size ip = stack.top();
        stack.pop();
        dieUnbalancedToken((*program)[ip].tok);
    }
}
