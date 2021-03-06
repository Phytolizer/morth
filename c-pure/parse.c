#include "parse.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

op_t parse_token_as_op(token_t token) {
    if (strcmp(token.text, "+") == 0) {
        return plus(token_dup(token));
    }
    if (strcmp(token.text, "-") == 0) {
        return minus(token_dup(token));
    }
    if (strcmp(token.text, "dump") == 0) {
        return dump(token_dup(token));
    }
    if (strcmp(token.text, "=") == 0) {
        return eq(token_dup(token));
    }
    if (strcmp(token.text, "if") == 0) {
        return iff(token_dup(token));
    }
    if (strcmp(token.text, "end") == 0) {
        return end(token_dup(token));
    }
    if (strcmp(token.text, "else") == 0) {
        return elze(token_dup(token));
    }
    if (strcmp(token.text, "dup") == 0) {
        return dupp(token_dup(token));
    }
    if (strcmp(token.text, "while") == 0) {
        return wile(token_dup(token));
    }
    if (strcmp(token.text, "do") == 0) {
        return doo(token_dup(token));
    }
    if (strcmp(token.text, ">") == 0) {
        return gt(token_dup(token));
    }
    if (strcmp(token.text, "<") == 0) {
        return lt(token_dup(token));
    }
    if (strcmp(token.text, "mem") == 0) {
        return mem(token_dup(token));
    }
    if (strcmp(token.text, ",") == 0) {
        return load(token_dup(token));
    }
    if (strcmp(token.text, ".") == 0) {
        return store(token_dup(token));
    }
    if (strcmp(token.text, "syscall1") == 0) {
        return syscall1(token_dup(token));
    }
    if (strcmp(token.text, "syscall3") == 0) {
        return syscall3(token_dup(token));
    }
    if (strcmp(token.text, "2dup") == 0) {
        return dupp2(token_dup(token));
    }
    if (strcmp(token.text, "drop") == 0) {
        return drop(token_dup(token));
    }
    if (strcmp(token.text, "shr") == 0) {
        return shr(token_dup(token));
    }
    if (strcmp(token.text, "shl") == 0) {
        return shl(token_dup(token));
    }
    if (strcmp(token.text, "bor") == 0) {
        return bor(token_dup(token));
    }
    if (strcmp(token.text, "band") == 0) {
        return band(token_dup(token));
    }
    if (strcmp(token.text, "swap") == 0) {
        return swap(token_dup(token));
    }
    if (strcmp(token.text, "over") == 0) {
        return over(token_dup(token));
    }

    char* endp;
    errno = 0;
    long long operand = strtoll(token.text, &endp, 10);
    if (errno != 0 || *endp != '\0' || operand < INT64_MIN || operand > INT64_MAX) {
        fprintf(stderr, "%s:%zu:%zu: Invalid token '%s'\n", token.loc.file_path, token.loc.row,
                token.loc.col, token.text);
        exit(EXIT_FAILURE);
    }

    return push((int64_t)operand, token_dup(token));
}
