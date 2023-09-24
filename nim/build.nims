#!/usr/bin/env -S nim e --hints:off
when defined(nimsuggest):
  import system/nimscript
import std/os
import std/sequtils
import std/strutils

proc execArray(args: openArray[string]) {.inline.} =
  args.mapIt(it.quoteShell).join(" ").exec

mode = ScriptMode.Verbose

template iife(body: untyped): untyped =
  (proc (): auto = body)()

let args = commandLineParams()
let scriptIdx = iife:
  for i, arg in args:
    if arg.extractFilename == "build.nims":
      return i

let restArgs = args[scriptIdx + 1 .. ^1]

execArray(@["nim", "c", "--outDir:bin", thisDir() & "/main.nim"] & restArgs)
