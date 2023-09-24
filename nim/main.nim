import std/os
import std/osproc
import std/sequtils
import std/strformat
import std/strutils

import pkg/compile
import pkg/op
import pkg/simulate

let program = @[
  push(34),
  push(35),
  plus(),
  dump(),
  push(500),
  push(80),
  minus(),
  dump(),
]

proc usage(f: File) =
  f.write fmt"""
    Usage: {getAppFilename()} <SUBCOMMAND> [ARGS]
    SUBCOMMANDS:
      sim                 Simulate the program
      com                 Compile the program
  """.dedent

proc exec(cmd: string) {.inline.} =
  echo cmd
  let res = execCmdEx(cmd)
  stdout.write res.output
  if res.exitCode != 0:
    raise newException(OSError, "crash")

proc main =
  if paramCount() < 1:
    usage(stderr)
    echo "ERROR: no subcommand provided"
    quit 1

  let subcommand = paramStr(1)

  case subcommand
  of "sim":
    simulateProgram(program)
  of "com":
    compileProgram(program, "output.asm")
    exec "nasm -felf64 output.asm"
    exec "ld -o output output.o"
  else:
    usage(stderr)
    echo "ERROR: unknown subcommand", subcommand
    quit 1

when isMainModule: main()
