import std/[
  enumerate,
  re,
  sequtils,
]

type
  Token* = object
    filePath*: string
    row*: int
    col*: int
    text*: string

func newToken(filePath: string, row: int, col: int, text: string): Token =
  result.filePath = filePath
  result.row = row
  result.col = col
  result.text = text

let
  WS_RE = re"\s"
  NWS_RE = re"\S"

func findCol(line: string, col: int, r: Regex): int =
  result = find(line.substr(col), r)
  if result == -1:
    return line.len
  return result + col

proc lexLine(filePath: string, row: int, line: string): seq[Token] =
  var col = findCol(line, 0, NWS_RE)
  while col != line.len:
    let colEnd = findCol(line, col, WS_RE)
    result.add(newToken(filePath, row, col + 1, line[col..<colEnd]))
    col = findCol(line, colEnd, NWS_RE)

proc lexFile*(filePath: string): seq[Token] =
  for (row, line) in enumerate(lines(filePath)):
    result = result.concat(lexLine(filePath, row + 1, line))
