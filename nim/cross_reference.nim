import op
import std/[
  enumerate,
]

static:
  assert int(OpCode.COUNT) == 10

proc crossReferenceBlocks*(program: var openArray[Op]) =
  var stack: seq[int] = @[]
  for (i, op) in enumerate(program):
    case op.code:
    of OpCode.IF:
      stack.add(i)
    of OpCode.ELSE:
      let ifIp = stack.pop()
      assert program[ifIp].code == OpCode.IF
      program[ifIp].operand = Word(i + 1)
      stack.add(i)
    of OpCode.END:
      let ifIp = stack.pop()
      case program[ifIp].code:
      of OpCode.IF, OpCode.ELSE:
        program[ifIp].operand = Word(i)
      else:
        raiseAssert("unreachable")
    else:
      discard
