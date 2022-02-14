import dataclasses
import enum


class OpCode(enum.Enum):
    PUSH = enum.auto()
    PLUS = enum.auto()
    MINUS = enum.auto()
    DUMP = enum.auto()


class SimulationError(Exception):
    pass


class CompilationError(Exception):
    pass


@dataclasses.dataclass(init=False)
class Op:
    code: OpCode
    value: int

    def __init__(self, code: OpCode, value: int = 0) -> None:
        self.code = code
        self.value = value


def push(x: int) -> Op:
    return Op(OpCode.PUSH, x)


def plus() -> Op:
    return Op(OpCode.PLUS)


def minus() -> Op:
    return Op(OpCode.MINUS)


def dump() -> Op:
    return Op(OpCode.DUMP)


def simulate_program(program: tuple[Op, ...]) -> None:
    stack: list[int] = []
    assert len(OpCode) == 4, "Exhaustive handling of OpCodes in simulate_program"
    for op in program:
        match op.code:
            case OpCode.PUSH:
                stack.append(op.value)
            case OpCode.PLUS:
                b = stack.pop()
                a = stack.pop()
                stack.append(a + b)
            case OpCode.MINUS:
                b = stack.pop()
                a = stack.pop()
                stack.append(a - b)
            case OpCode.DUMP:
                a = stack.pop()
                print(a)


def compile_program(program: tuple[Op, ...]) -> None:
    raise CompilationError("Not implemented")
