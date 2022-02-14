from morth import dump, minus, plus, push, simulate_program

PROGRAM = (
    push(34),
    push(35),
    plus(),
    dump(),
    push(500),
    push(80),
    minus(),
    dump(),
)
simulate_program(PROGRAM)
