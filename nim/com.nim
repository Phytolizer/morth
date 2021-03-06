import op
import mem
import std/[
  enumerate,
  strformat,
]

static:
  assert int(OpCode.COUNT) == 15

const STACK_LL = readFile("stack.ll")

var register: uint64 = 0

proc allocateRegister: uint64 =
  result = register
  register += 1

proc compileProgram*(program: openArray[Op], outPath: string) =
  let f = open(outPath, fmWrite)
  defer: f.close()

  f.writeLine(STACK_LL)
  f.writeLine("; Generated code follows.")
  f.writeLine("")
  f.writeLine(fmt"@mem = global [{MEM_CAPACITY} x i8] undef")
  f.writeLine("")
  f.writeLine("declare void @dump(i64)")
  f.writeLine("")
  f.writeLine("define i64 @main() {")
  for (i, op) in enumerate(program):
    f.writeLine(fmt"MorthInstr{i}:")
    case op.code:
    of OpCode.PUSH:
      f.writeLine(fmt"  call void(i64) @push(i64 {op.operand})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.PLUS:
      let b = allocateRegister()
      f.writeLine(fmt"  %{b} = call i64() @pop()")
      let a = allocateRegister()
      f.writeLine(fmt"  %{a} = call i64() @pop()")
      let res = allocateRegister()
      f.writeLine(fmt"  %{res} = add i64 %{a}, %{b}")
      f.writeLine(fmt"  call void(i64) @push(i64 %{res})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.MINUS:
      let b = allocateRegister()
      f.writeLine(fmt"  %{b} = call i64() @pop()")
      let a = allocateRegister()
      f.writeLine(fmt"  %{a} = call i64() @pop()")
      let res = allocateRegister()
      f.writeLine(fmt"  %{res} = sub i64 %{a}, %{b}")
      f.writeLine(fmt"  call void(i64) @push(i64 %{res})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.DUMP:
      let val = allocateRegister()
      f.writeLine(fmt"  %{val} = call i64() @pop()")
      f.writeLine(fmt"  call void(i64) @dump(i64 %{val})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.EQ:
      let b = allocateRegister()
      f.writeLine(fmt"  %{b} = call i64() @pop()")
      let a = allocateRegister()
      f.writeLine(fmt"  %{a} = call i64() @pop()")
      let res = allocateRegister()
      f.writeLine(fmt"  %{res} = icmp eq i64 %{a}, %{b}")
      let res2 = allocateRegister()
      f.writeLine(fmt"  %{res2} = zext i1 %{res} to i64")
      f.writeLine(fmt"  call void(i64) @push(i64 %{res2})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.IF:
      let val = allocateRegister()
      f.writeLine(fmt"  %{val} = call i64 @pop()")
      let res = allocateRegister()
      f.writeLine(fmt"  %{res} = icmp eq i64 %{val}, 0")
      f.writeLine(fmt"  br i1 %{res}, label %MorthInstr{op.operand}, label %MorthInstr{i + 1}")
    of OpCode.END:
      f.writeLine(fmt"  br label %MorthInstr{op.operand}")
    of OpCode.ELSE:
      f.writeLine(fmt"  br label %MorthInstr{op.operand}")
    of OpCode.DUP:
      let a = allocateRegister()
      f.writeLine(fmt"  %{a} = call i64() @pop()")
      f.writeLine(fmt"  call void(i64) @push(i64 %{a})")
      f.writeLine(fmt"  call void(i64) @push(i64 %{a})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.GT:
      let b = allocateRegister()
      f.writeLine(fmt"  %{b} = call i64() @pop()")
      let a = allocateRegister()
      f.writeLine(fmt"  %{a} = call i64() @pop()")
      let res = allocateRegister()
      f.writeLine(fmt"  %{res} = icmp ugt i64 %{a}, %{b}")
      let res2 = allocateRegister()
      f.writeLine(fmt"  %{res2} = zext i1 %{res} to i64")
      f.writeLine(fmt"  call void(i64) @push(i64 %{res2})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.WHILE:
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.DO:
      let val = allocateRegister()
      f.writeLine(fmt"  %{val} = call i64 @pop()")
      let res = allocateRegister()
      f.writeLine(fmt"  %{res} = icmp eq i64 %{val}, 0")
      f.writeLine(fmt"  br i1 %{res}, label %MorthInstr{op.operand}, label %MorthInstr{i + 1}")
    of OpCode.MEM:
      let tmp = allocateRegister()
      f.writeLine(fmt"  %{tmp} = ptrtoint [{MEM_CAPACITY} x i8]* @mem to i64")
      f.writeLine(fmt"  call void(i64) @push(i64 %{tmp})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.LOAD:
      let address = allocateRegister()
      f.writeLine(fmt"  %{address} = call i64() @pop()")
      let p = allocateRegister()
      f.writeLine(fmt"  %{p} = inttoptr i64 %{address} to i8*")
      let val = allocateRegister()
      f.writeLine(fmt"  %{val} = load i8, i8* %{p}")
      let extVal = allocateRegister()
      f.writeLine(fmt"  %{extVal} = sext i8 %{val} to i64")
      f.writeLine(fmt"  call void(i64) @push(i64 %{extVal})")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    of OpCode.STORE:
      let val = allocateRegister()
      f.writeLine(fmt"  %{val} = call i64() @pop()")
      let address = allocateRegister()
      f.writeLine(fmt"  %{address} = call i64() @pop()")
      let p = allocateRegister()
      f.writeLine(fmt"  %{p} = inttoptr i64 %{address} to i8*")
      let valTrunc = allocateRegister()
      f.writeLine(fmt"  %{valTrunc} = trunc i64 %{val} to i8")
      f.writeLine(fmt"  store i8 %{valTrunc}, i8* %{p}")
      f.writeLine(fmt"  br label %MorthInstr{i + 1}")
    else:
      raiseAssert("unreachable")
  f.writeLine(fmt"MorthInstr{program.len}:")
  f.writeLine("  ret i64 0")
  f.writeLine("}")
