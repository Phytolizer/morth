#ifndef MORTH_STACK_H_
#define MORTH_STACK_H_

#include <stddef.h>
#include <stdint.h>

typedef struct {
    uint64_t* data;
    size_t size;
    size_t capacity;
} stack_t;

#define STACK_INIT                                                                                 \
    { .data = NULL, .size = 0, .capacity = 0 }

void stack_push(stack_t* stack, uint64_t value);
uint64_t stack_pop(stack_t* stack);

#endif
