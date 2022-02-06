import std/[
  strformat,
]

type
  OpCode = enum
    OpPush
    OpPlus
    OpMinus
    OpDump

  Op = object
    case code: OpCode
    of OpPush:
      arg: uint64
    else:
      discard

func push(x: uint64): Op = Op(code: OpPush, arg: x)
func plus(): Op = Op(code: OpPlus)
func minus(): Op = Op(code: OpMinus)
func dump(): Op = Op(code: OpDump)

const
  program = @[
    push(34),
    push(35),
    plus(),
    dump(),
    push(500),
    push(80),
    minus(),
    dump(),
  ]

proc simulateProgram(program: seq[Op]) =
  var ip = 0
  var stack: seq[uint64] = @[]
  while ip < program.len:
    case program[ip].code
    of OpPush:
      stack.add(program[ip].arg)
      ip += 1
    of OpPlus:
      let b = stack.pop
      let a = stack.pop
      stack.add(a + b)
      ip += 1
    of OpMinus:
      let b = stack.pop
      let a = stack.pop
      stack.add(a - b)
      ip += 1
    of OpDump:
      let x = stack.pop
      echo x
      ip += 1

proc compileProgram(program: seq[Op], outFilePath: string) =
  var outFile = open(outFilePath, fmWrite)
  defer: outFile.close()

  outFile.writeLine "segment .text"
  outFile.writeLine "dump:"
  outFile.writeLine "sub     rsp, 40"
  outFile.writeLine "lea     rsi, [rsp + 31]"
  outFile.writeLine "mov     byte [rsp + 31], 10"
  outFile.writeLine "mov     ecx, 1"
  outFile.writeLine "mov     r8, -3689348814741910323"
  outFile.writeLine ".LBB0_1:"
  outFile.writeLine "mov     rax, rdi"
  outFile.writeLine "mul     r8"
  outFile.writeLine "shr     rdx, 3"
  outFile.writeLine "lea     eax, [rdx + rdx]"
  outFile.writeLine "lea     r9d, [rax + 4*rax]"
  outFile.writeLine "mov     eax, edi"
  outFile.writeLine "sub     eax, r9d"
  outFile.writeLine "or      al, 48"
  outFile.writeLine "mov     byte [rsi - 1], al"
  outFile.writeLine "add     rsi, -1"
  outFile.writeLine "add     rcx, 1"
  outFile.writeLine "cmp     rdi, 9"
  outFile.writeLine "mov     rdi, rdx"
  outFile.writeLine "ja      .LBB0_1"
  outFile.writeLine "mov     edi, 1"
  outFile.writeLine "mov     rdx, rcx"
  outFile.writeLine "mov     rax, 1"
  outFile.writeLine "syscall"
  outFile.writeLine "add     rsp, 40"
  outFile.writeLine "ret"
  outFile.writeLine "global _start"
  outFile.writeLine "_start:"
  for op in program:
    case op.code
    of OpPush:
      outFile.writeLine fmt"push {op.arg}"
    of OpPlus:
      outFile.writeLine "pop rbx"
      outFile.writeLine "pop rax"
      outFile.writeLine "add rax, rbx"
      outFile.writeLine "push rax"
    of OpMinus:
      outFile.writeLine "pop rbx"
      outFile.writeLine "pop rax"
      outFile.writeLine "sub rax, rbx"
      outFile.writeLine "push rax"
    of OpDump:
      outFile.writeLine "pop rdi"
      outFile.writeLine "call dump"
  outFile.writeLine "mov rax, 60"
  outFile.writeLine "mov rdi, 0"
  outFile.writeLine "syscall"

when isMainModule:
  simulateProgram(program)
  compileProgram(program, "output.asm")
