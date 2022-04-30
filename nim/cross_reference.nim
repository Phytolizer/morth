import op
import std/[
  enumerate,
  strformat,
]

static:
  assert int(OpCode.COUNT) == 13

proc crossReferenceBlocks*(program: var openArray[Op]) =
  var stack: seq[int] = @[]
  for (i, op) in enumerate(program):
    case op.code:
    of OpCode.IF:
      stack.add(i)
    of OpCode.ELSE:
      let ifIp = stack.pop()
      if program[ifIp].code != OpCode.IF:
        let tok = program[ifIp].token
        stderr.writeLine fmt"{tok.filePath}:{tok.row}:{tok.col}: ERROR: `else` can only be used with `if`"
        quit 1
      program[ifIp].operand = Word(i + 1)
      stack.add(i)
    of OpCode.END:
      let blockIp = stack.pop()
      case program[blockIp].code:
      of OpCode.IF, OpCode.ELSE:
        program[blockIp].operand = Word(i)
        program[i].operand = Word(i + 1)
      of OpCode.DO:
        program[i].operand = program[blockIp].operand
        program[blockIp].operand = Word(i + 1)
      else:
        let tok = program[blockIp].token
        stderr.writeLine fmt"{tok.filePath}:{tok.row}:{tok.col}: ERROR: `end` cannot close `{tok.text}`"
        quit 1
    of OpCode.WHILE:
      stack.add(i)
    of OpCode.DO:
      let whileIp = stack.pop()
      program[i].operand = Word(whileIp)
      stack.add(i)
    else:
      discard

  if stack.len() > 0:
    let tok = program[stack.pop()].token
    stderr.writeLine fmt"{tok.filePath}:{tok.row}:{tok.col}: ERROR: unclosed `{tok.text}`"
    quit 1
