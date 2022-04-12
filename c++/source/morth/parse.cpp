#include "morth/parse.hpp"

#include <charconv>
#include <fmt/format.h>
#include <stdexcept>

Op ParseTokenAsOp(Token tok) {
    if (tok.text == "+") {
        return Plus(tok);
    }
    if (tok.text == "-") {
        return Minus(tok);
    }
    if (tok.text == "dump") {
        return Dump(tok);
    }

    Int64 value;
    auto result = std::from_chars(tok.text.data(), tok.text.data() + tok.text.size(), value);
    if (result.ec != std::errc{}) {
        fmt::print(stderr, "{}:{}:{}: Invalid token '{}'\n", tok.location.filePath,
                   tok.location.row, tok.location.column, tok.text);
        throw std::runtime_error{"Invalid token"};
    }

    return Push(value, tok);
}
