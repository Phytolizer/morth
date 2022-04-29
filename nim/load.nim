import lex
import op
import parse

proc loadProgramFromFile*(filePath: string): seq[Op] =
  for tok in lexFile(filePath):
    result.add(parseTokenAsOp(tok))
