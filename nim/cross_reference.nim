import op
import std/[
  enumerate,
]

proc crossReferenceBlocks*(program: var openArray[Op]) =
  var stack: seq[int] = @[]
  for (i, op) in enumerate(program):
    case op.code:
    of OpCode.IF:
      stack.add(i)
    of OpCode.END:
      let ifIp = stack.pop()
      assert program[ifIp].code == OpCode.IF
      program[ifIp].operand = Word(i)
    else:
      discard
