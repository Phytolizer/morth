import pkg/op


proc simulateProgram*(program: Program) =
  var stack = newSeqOfCap[int](8)

  template binaryOp(op: proc(a, b: int): int): untyped =
    let b = stack.pop()
    let a = stack.pop()
    stack.add(op(a, b))

  for op in program:
    case op.kind
    of OpKind.Push: stack.add(op.push)
    of OpKind.PLUS: binaryOp(`+`)
    of OpKind.MINUS: binaryOp(`-`)
    of OpKind.Dump: echo stack.pop()
