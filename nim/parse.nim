import op
import lex
import std/[
  strformat,
  strutils,
]

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
  else:
    try:
      return opPush(parseBiggestUInt(tok.text))
    except ValueError as e:
      stderr.writeLine(fmt"{tok.filePath}:{tok.row}:{tok.col}: {e.msg}")
      quit 1
