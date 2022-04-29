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

proc cmdEchoed(cmd: string) =
  echo "[CMD] " & cmd
  discard execShellCmd(cmd)

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
    let (dir, file, _) = splitFile(filePath)
    let outputPath = dir & "/" & file
    let llPath = outputPath & ".ll"
    compileProgram(program, llPath)
    cmdEchoed(fmt"clang -o {outputPath}{ExeExt} {llPath} dump.ll")
  of "help":
    usage()
  else:
    usage()
    stderr.writeLine "ERROR: unknown subcommand `" & paramStr(1) & "`"
    quit 1
