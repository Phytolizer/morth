import op

proc simulateProgram*(program: openArray[Op]) =
  assert int(OpCode.COUNT) == 4
  var stack: seq[Word] = @[]
  for op in program:
    if op.code == OpCode.PUSH:
      stack.add(op.operand)
    elif op.code == OpCode.PLUS:
      let b = stack.pop()
      let a = stack.pop()
      stack.add(a + b)
    elif op.code == OpCode.MINUS:
      let b = stack.pop()
      let a = stack.pop()
      stack.add(a - b)
    elif op.code == OpCode.DUMP:
      let val = stack.pop()
      echo val
    else:
      raiseAssert("unreachable")
