#pragma once

#include "source_location.h"

#include <stddef.h>

typedef struct {
    char* text;
    source_location_t loc;
} token_t;

token_t token_dup(token_t tok);
void token_free(token_t tok);
