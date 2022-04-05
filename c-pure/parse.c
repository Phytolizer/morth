#include "parse.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

op_t parse_token_as_op(token_t token) {
    if (strcmp(token.text, "+") == 0) {
        return plus();
    }
    if (strcmp(token.text, "-") == 0) {
        return minus();
    }
    if (strcmp(token.text, ".") == 0) {
        return dump();
    }

    char* endp;
    errno = 0;
    long long operand = strtoll(token.text, &endp, 10);
    if (errno != 0 || *endp != '\0' || operand < INT64_MIN || operand > INT64_MAX) {
        fprintf(stderr, "%s:%zu:%zu: Invalid token %s\n", token.file_path, token.row, token.col,
                token.text);
        exit(EXIT_FAILURE);
    }

    return push((int64_t)operand);
}
