#pragma once

#include "buffer.h"

#include <stdint.h>

typedef BUFFER_TYPE(int64_t) stack_t;

#define STACK_INIT (stack_t) BUFFER_INIT
#define STACK_FREE(stack) BUFFER_FREE(stack)

void stack_push(stack_t* stack, int64_t value);
int64_t stack_pop(stack_t* stack);
