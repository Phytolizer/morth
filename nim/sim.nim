import op

static:
  assert int(OpCode.COUNT) == 7

proc simulateProgram*(program: openArray[Op]) =
  var stack: seq[Word] = @[]
  var i: Word = 0
  while i < Word(program.len):
    let op = program[i]
    case op.code:
    of OpCode.PUSH:
      stack.add(op.operand)
      i += 1
    of OpCode.PLUS:
      let b = stack.pop()
      let a = stack.pop()
      stack.add(a + b)
      i += 1
    of OpCode.MINUS:
      let b = stack.pop()
      let a = stack.pop()
      stack.add(a - b)
      i += 1
    of OpCode.DUMP:
      let val = stack.pop()
      echo val
      i += 1
    of OpCode.EQ:
      let b = stack.pop()
      let a = stack.pop()
      stack.add(Word(a == b))
      i += 1
    of OpCode.IF:
      let a = stack.pop()
      if a == 0:
        i = op.operand
      else:
        i += 1
    of OpCode.END:
      i += 1
    else:
      raiseAssert("unreachable")
