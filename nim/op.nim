type
  OpCode* {.pure.} = enum
    PUSH
    PLUS
    DUMP

  Op* = object
    code*: OpCode

