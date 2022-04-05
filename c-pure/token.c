#include "token.h"

#include <stdlib.h>
#include <string.h>

token_t token_dup(token_t tok) {
    token_t result;
    result.text = strdup(tok.text);
    result.loc = tok.loc;
    return result;
}

void token_free(token_t tok) {
    free(tok.text);
}
