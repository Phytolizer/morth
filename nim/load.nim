import lex
import op
import parse
import cross_reference

proc loadProgramFromFile*(filePath: string): seq[Op] =
  for tok in lexFile(filePath):
    result.add(parseTokenAsOp(tok))
  crossReferenceBlocks(result)
