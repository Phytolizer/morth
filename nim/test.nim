import std/[
  os,
  osproc,
  strformat,
  strutils,
]

proc cmdEchoed(capture: bool, cmd: string, args: varargs[string]): string =
  let cmdJoined = cmd & " " & args.join(" ")
  echo fmt"[CMD] {cmdJoined}"
  if capture:
    let (output, _) = execCmdEx(cmdJoined)
    return output
  discard execCmd(cmdJoined)
  return ""

func exePath(path: string): string =
  if ExeExt == "":
    return path
  return path & "." & ExeExt

when isMainModule:
  discard cmdEchoed(false, "nim", "c", "morth.nim")
  let morthExe = joinPath(".", exePath("morth"))
  for entry in walkDir("tests"):
    if entry.kind == pcFile and entry.path.endsWith(".morth"):
      echo fmt"[INFO] Testing {entry.path}"
      let simOutput = cmdEchoed(true, morthExe, "sim", entry.path)
      discard cmdEchoed(false, morthExe, "com", "-o", "morth_test", entry.path)
      let comOutput = cmdEchoed(true, joinPath(".", "morth_test"))
      if simOutput != comOutput:
        stderr.writeLine "[ERROR] Output discrepancy between simulation and compilation"
        stderr.writeLine "  Simulation:"
        stderr.writeLine simOutput.indent(4)
        stderr.writeLine "  Compilation:"
        stderr.writeLine comOutput.indent(4)
        quit 1
      echo fmt"[INFO] {entry.path} OK"
