type
  OpCode = enum
    OpPush
    OpPlus
    OpDump

  Op = object
    case code: OpCode
    of OpPush:
      arg: uint64
    else:
      discard

const
  program = @[
    Op(code: OpPush, arg: 34),
    Op(code: OpPush, arg: 35),
    Op(code: OpPlus),
    Op(code: OpDump),
  ]

proc simulateProgram(program: seq[Op]) =
  var ip = 0
  var stack: seq[uint64] = @[]
  while ip < program.len:
    case program[ip].code
    of OpPush:
      stack.add(program[ip].arg)
      ip += 1
    of OpPlus:
      let b = stack.pop
      let a = stack.pop
      stack.add(a + b)
      ip += 1
    of OpDump:
      let x = stack.pop
      echo x
      ip += 1

when isMainModule:
  simulateProgram(program)
