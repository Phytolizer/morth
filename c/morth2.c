#include "./op.h"
#include "./simulate.h"

int main(void) {
    op_t program[8];
    program[0] = op_push(34);
    program[1] = op_push(35);
    program[2] = op_plus();
    program[3] = op_dump();
    program[4] = op_push(500);
    program[5] = op_push(80);
    program[6] = op_minus();
    program[7] = op_dump();

    simulate_program((program_t){
        .data = program,
        .size = sizeof program / sizeof(op_t),
    });

    return 0;
}
