#include "token.h"

#include <stdlib.h>
#include <string.h>

token_t token_dup(token_t tok) {
    token_t result;
    size_t len = strlen(tok.text);
    result.text = malloc(len + 1);
    memcpy(result.text, tok.text, len + 1);
    result.loc = tok.loc;
    return result;
}

void token_free(token_t tok) {
    free(tok.text);
}
