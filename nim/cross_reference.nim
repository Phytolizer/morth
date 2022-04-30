import op
import std/[
  enumerate,
]

static:
  assert int(OpCode.COUNT) == 12

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
      let blockIp = stack.pop()
      case program[blockIp].code:
      of OpCode.IF, OpCode.ELSE:
        program[blockIp].operand = Word(i)
        program[i].operand = Word(i + 1)
      of OpCode.DO:
        program[i].operand = program[blockIp].operand
        program[blockIp].operand = Word(i + 1)
      else:
        raiseAssert("unreachable")
    of OpCode.WHILE:
      stack.add(i)
    of OpCode.DO:
      let whileIp = stack.pop()
      program[i].operand = Word(whileIp)
      stack.add(i)
    else:
      discard
