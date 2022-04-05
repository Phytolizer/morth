#include "parse.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

op_t parse_token_as_op(const char* token) {
    if (strcmp(token, "+") == 0) {
        return plus();
    }
    if (strcmp(token, "-") == 0) {
        return minus();
    }
    if (strcmp(token, ".") == 0) {
        return dump();
    }

    char* endp;
    errno = 0;
    long long operand = strtoll(token, &endp, 10);
    if (errno != 0 || *endp != '\0' || operand < INT64_MIN || operand > INT64_MAX) {
        fprintf(stderr, "[ERROR] Invalid token %s\n", token);
        exit(1);
    }

    return push((int64_t)operand);
}
