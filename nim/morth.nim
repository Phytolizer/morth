import op
import sim
import com
import std/[
  os,
  strformat,
]

const
  PROGRAM = [
    opPush(34),
    opPush(35),
    opPlus(),
    opDump(),
    opPush(500),
    opPush(80),
    opMinus(),
    opDump(),
  ]

proc usage =
  stderr.writeLine "Usage: morth <SUBCOMMAND> [ARGS]"
  stderr.writeLine "SUBCOMMANDS:"
  stderr.writeLine "  sim             Simulate the program"
  stderr.writeLine "  com             Compile the program"

when isMainModule:
  if paramCount() < 1:
    usage()
    stderr.writeLine "ERROR: no subcommand provided"
    quit 1

  case paramStr(1):
  of "sim":
    simulateProgram(PROGRAM)
  of "com":
    compileProgram(PROGRAM)
    discard execShellCmd(fmt"clang -o output{ExeExt} output.ll")
  else:
    usage()
    stderr.writeLine "ERROR: unknown subcommand `" & paramStr(1) & "`"
    quit 1
