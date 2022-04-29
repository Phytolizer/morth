import op

static:
  assert int(OpCode.COUNT) == 4

proc simulateProgram*(program: openArray[Op]) =
  var stack: seq[Word] = @[]
  for op in program:
    case op.code:
    of OpCode.PUSH:
      stack.add(op.operand)
    of OpCode.PLUS:
      let b = stack.pop()
      let a = stack.pop()
      stack.add(a + b)
    of OpCode.MINUS:
      let b = stack.pop()
      let a = stack.pop()
      stack.add(a - b)
    of OpCode.DUMP:
      let val = stack.pop()
      echo val
    else:
      raiseAssert("unreachable")
