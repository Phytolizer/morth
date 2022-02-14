import argparse

from morth import compile_program, dump, minus, plus, push, simulate_program

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

parser = argparse.ArgumentParser()
subcommands = parser.add_subparsers(title="subcommands")
sim_subcommand = subcommands.add_parser("sim", help="Simulate the program")
sim_subcommand.set_defaults(func=simulate_program)
com_subcommand = subcommands.add_parser("com", help="Compile the program")
com_subcommand.set_defaults(func=compile_program)

args = parser.parse_args()
args.func(PROGRAM)
