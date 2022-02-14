import enum
import dataclasses


class OpCode(enum.Enum):
    PUSH = enum.auto()
    PLUS = enum.auto()
    DUMP = enum.auto()
    COUNT = enum.auto()


@dataclasses.dataclass(init=False)
class Op:
    def __init__(self, code: OpCode, value: int = None) -> None:
        self.code = code
        self.value = value


def push(x: int) -> Op:
    return Op(OpCode.PUSH, x)


def plus() -> Op:
    return Op(OpCode.PLUS)


def dump() -> Op:
    return Op(OpCode.DUMP)


def simulate_program(program: tuple[Op, ...]) -> None:
    assert False, "Not implemented"


def compile_program(program: tuple[Op, ...]) -> None:
    assert False, "Not implemented"


PROGRAM = (
    push(34),
    push(35),
    plus(),
    dump(),
)
simulate_program(PROGRAM)
