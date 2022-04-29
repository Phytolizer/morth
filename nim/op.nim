type
  Word* = int64

  OpCode* {.pure.} = enum
    PUSH
    PLUS
    MINUS
    DUMP
    COUNT

  Op* = object
    code*: OpCode
    operand*: Word

func opPush*(x: Word): Op =
  result.code = OpCode.PUSH
  result.operand = x

func opPlus*: Op =
  result.code = OpCode.PLUS

func opMinus*: Op =
  result.code = OpCode.MINUS

func opDump*: Op =
  result.code = OpCode.DUMP
