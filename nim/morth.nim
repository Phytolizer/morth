import sim
import com
import load
import std/[
  os,
  strformat,
  tempfiles,
]

const DUMP_C = readFile("assembly_src/dump.c")

proc usage =
  stderr.writeLine fmt"Usage: {getAppFilename()} <SUBCOMMAND> [ARGS]"
  stderr.writeLine "SUBCOMMANDS:"
  stderr.writeLine "  sim             Simulate the program"
  stderr.writeLine "  com             Compile the program"
  stderr.writeLine "  help            Print this help message"

proc cmdEchoed(cmd: string) =
  echo "[CMD] " & cmd
  discard execShellCmd(cmd)

func exePath(path: string): string =
  if ExeExt.len > 0:
    path & "." & ExeExt
  else:
    path

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
    echo fmt"[INFO] Generating {llPath}"
    compileProgram(program, llPath)
    let (cfile, dumpPath) = createTempFile("morth_", ".c")
    cfile.write(DUMP_C)
    cfile.close()
    cmdEchoed(fmt"clang -o {exePath(outputPath)} {llPath} {dumpPath}")
  of "help":
    usage()
  else:
    usage()
    stderr.writeLine "ERROR: unknown subcommand `" & paramStr(1) & "`"
    quit 1
