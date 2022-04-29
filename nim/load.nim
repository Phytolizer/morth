import parse
import op
import std/[
  strutils,
]

proc loadProgramFromFile*(filePath: string): seq[Op] =
  for line in lines(filePath):
    for word in line.splitWhitespace():
      result.add(parseWordAsOp(word))
