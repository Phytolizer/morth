import op
import std/[
  strutils,
]

func parseWordAsOp*(word: string): Op =
  case word:
  of "+":
    return opPlus()
  of "-":
    return opMinus()
  of ".":
    return opDump()
  else:
    return opPush(parseBiggestUInt(word))
