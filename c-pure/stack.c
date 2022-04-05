#include "stack.h"

#include <assert.h>

void stack_push(stack_t* stack, int64_t value) {
    BUFFER_EXPAND(stack);
    stack->data[stack->length] = value;
    stack->length++;
}

int64_t stack_pop(stack_t* stack) {
    assert(stack->length > 0);
    stack->length--;
    return stack->data[stack->length];
}
