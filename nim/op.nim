type OpKind* {.pure.} = enum
  Push
  Plus
  Minus
  Dump

type Op* = object
  case kind*: OpKind
  of Push:
    push*: int
  else: discard

func push*(x: int): Op = Op(kind: Push, push: x)
func plus*: Op = Op(kind: Plus)
func minus*: Op = Op(kind: Minus)
func dump*: Op = Op(kind: Dump)

type Program* = seq[Op]
