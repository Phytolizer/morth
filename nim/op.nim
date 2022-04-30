import lex

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
    GT
    WHILE
    DO
    MEM
    COUNT

  Op* = object
    token*: Token
    code*: OpCode
    operand*: Word

func opPush*(token: Token, x: Word): Op =
  result.token = token
  result.code = OpCode.PUSH
  result.operand = x

func opPlus*(token: Token): Op =
  result.token = token
  result.code = OpCode.PLUS

func opMinus*(token: Token): Op =
  result.token = token
  result.code = OpCode.MINUS

func opDump*(token: Token): Op =
  result.token = token
  result.code = OpCode.DUMP

func opEq*(token: Token): Op =
  result.token = token
  result.code = OpCode.EQ

func opIf*(token: Token): Op =
  result.token = token
  result.code = OpCode.IF

func opEnd*(token: Token): Op =
  result.token = token
  result.code = OpCode.END

func opElse*(token: Token): Op =
  result.token = token
  result.code = OpCode.ELSE

func opDup*(token: Token): Op =
  result.token = token
  result.code = OpCode.DUP

func opGt*(token: Token): Op =
  result.token = token
  result.code = OpCode.GT

func opWhile*(token: Token): Op =
  result.token = token
  result.code = OpCode.WHILE

func opDo*(token: Token): Op =
  result.token = token
  result.code = OpCode.DO

func opMem*(token: Token): Op =
  result.token = token
  result.code = OpCode.MEM
