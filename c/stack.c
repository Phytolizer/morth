#include "./stack.h"

#include <stdio.h>
#include <stdlib.h>

void stack_push(stack_t* stack, uint64_t value) {
    if (stack->size == stack->capacity) {
        stack->capacity = stack->capacity * 2 + 1;
        uint64_t* new_data = realloc(stack->data, stack->capacity * sizeof(uint64_t));
        if (new_data == NULL) {
            fprintf(stderr, "Allocation failure on stack reallocation\n");
            free(stack->data);
            exit(1);
        }
        stack->data = new_data;
    }

    stack->data[stack->size] = value;
    stack->size += 1;
}

uint64_t stack_pop(stack_t* stack) {
    if (stack->size == 0) {
        fprintf(stderr, "Stack underflow\n");
        free(stack->data);
        exit(1);
    }

    stack->size -= 1;
    return stack->data[stack->size];
}
