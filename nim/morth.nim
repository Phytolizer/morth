import op
import sim
import com
import load
import std/[
  os,
  strformat,
]

proc usage =
  stderr.writeLine fmt"Usage: {getAppFilename()} <SUBCOMMAND> [ARGS]"
  stderr.writeLine "SUBCOMMANDS:"
  stderr.writeLine "  sim             Simulate the program"
  stderr.writeLine "  com             Compile the program"
  stderr.writeLine "  help            Print this help message"

when isMainModule:
  if paramCount() < 1:
    usage()
    stderr.writeLine "ERROR: no subcommand provided"
    quit 1

  var i = 1
  let subcommand = paramStr(i)
  i += 1

  case subcommand:
  of "sim":
    if paramCount() < i:
      usage()
      stderr.writeLine "ERROR: no input file provided"
      quit 1
    let filePath = paramStr(i)
    i += 1

    let program = loadProgramFromFile(filePath)
    simulateProgram(program)
  of "com":
    if paramCount() < i:
      usage()
      stderr.writeLine "ERROR: no input file provided"
      quit 1
    let filePath = paramStr(i)
    i += 1

    let program = loadProgramFromFile(filePath)
    compileProgram(program)
    discard execShellCmd(fmt"clang -o output{ExeExt} output.ll dump.ll")
  of "help":
    usage()
  else:
    usage()
    stderr.writeLine "ERROR: unknown subcommand `" & paramStr(1) & "`"
    quit 1
