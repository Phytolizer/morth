type
  Word* = uint64

  OpCode* {.pure.} = enum
    PUSH
    PLUS
    MINUS
    DUMP
    EQ
    IF
    END
    ELSE
    DUP
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

func opEq*: Op =
  result.code = OpCode.EQ

func opIf*: Op =
  result.code = OpCode.IF

func opEnd*: Op =
  result.code = OpCode.END

func opElse*: Op =
  result.code = OpCode.ELSE

func opDup*: Op =
  result.code = OpCode.DUP
