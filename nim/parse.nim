import op
import lex
import std/[
  strformat,
  strutils,
]

static:
  assert int(OpCode.COUNT) == 15

proc parseTokenAsOp*(tok: Token): Op =
  case tok.text:
  of "+":
    return opPlus(tok)
  of "-":
    return opMinus(tok)
  of "dump":
    return opDump(tok)
  of "=":
    return opEq(tok)
  of "if":
    return opIf(tok)
  of "end":
    return opEnd(tok)
  of "else":
    return opElse(tok)
  of "dup":
    return opDup(tok)
  of ">":
    return opGt(tok)
  of "while":
    return opWhile(tok)
  of "do":
    return opDo(tok)
  of "mem":
    return opMem(tok)
  of ",":
    return opLoad(tok)
  of ".":
    return opStore(tok)
  else:
    try:
      return opPush(tok, parseBiggestUInt(tok.text))
    except ValueError as e:
      stderr.writeLine(fmt"{tok.filePath}:{tok.row}:{tok.col}: {e.msg}")
      quit 1
