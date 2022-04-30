import op
import lex
import std/[
  strformat,
  strutils,
]

static:
  assert int(OpCode.COUNT) == 9

proc parseTokenAsOp*(tok: Token): Op =
  case tok.text:
  of "+":
    return opPlus()
  of "-":
    return opMinus()
  of ".":
    return opDump()
  of "=":
    return opEq()
  of "if":
    return opIf()
  of "end":
    return opEnd()
  of "else":
    return opElse()
  of "dup":
    return opDup()
  else:
    try:
      return opPush(parseBiggestUInt(tok.text))
    except ValueError as e:
      stderr.writeLine(fmt"{tok.filePath}:{tok.row}:{tok.col}: {e.msg}")
      quit 1
