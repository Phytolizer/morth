import std/strformat
import std/strutils

import pkg/op

const dumpAsm = staticRead("asm_sources/dump.nasm")

proc compileProgram*(program: Program, outPath: string) =
  var f = open(outPath, fmWrite)
  defer: close(f)
  f.write dumpAsm
  f.write dedent"""
        global _start
    _start:
  """
  for op in program:
    f.writeLine fmt"    ;; -- {op} --"
    case op.kind
    of OpKind.Push:
      f.write fmt"""
          push {op.push}
      """.dedent(6)
    of OpKind.Plus:
      f.write """
          pop rbx
          pop rax
          add rax, rbx
          push rax
      """.dedent(6)
    of OpKind.Minus:
      f.write """
          pop rbx
          pop rax
          sub rax, rbx
          push rax
      """.dedent(6)
    of OpKind.Dump:
      f.write fmt"""
          pop rdi
          call dump
      """.dedent(6)
  f.write """
      mov rax, 60
      mov rdi, 0
      syscall
  """.dedent(2)
