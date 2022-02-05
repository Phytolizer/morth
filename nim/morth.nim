type
  OpCode = enum
    OpPush
    OpPlus
    OpMinus
    OpDump

  Op = object
    case code: OpCode
    of OpPush:
      arg: uint64
    else:
      discard

func push(x: uint64): Op = Op(code: OpPush, arg: x)
func plus(): Op = Op(code: OpPlus)
func minus(): Op = Op(code: OpMinus)
func dump(): Op = Op(code: OpDump)

const
  program = @[
    push(34),
    push(35),
    plus(),
    dump(),
    push(500),
    push(80),
    minus(),
    dump(),
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
    of OpMinus:
      let b = stack.pop
      let a = stack.pop
      stack.add(a - b)
      ip += 1
    of OpDump:
      let x = stack.pop
      echo x
      ip += 1

when isMainModule:
  simulateProgram(program)
