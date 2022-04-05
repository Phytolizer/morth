#include "simulate.h"

#include <stdio.h>

int main(void) {
    op_t program[] = {
            push(34),
            push(35),
            plus(),
            dump(),
    };
    simulate_program(PROGRAM_FROM_ARRAY(program));
}
