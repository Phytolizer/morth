# frozen_string_literal: true

require "English"

class AssertionError < RuntimeError; end

def assert(message, &_)
  raise AssertionError, message unless yield
end

def iota(args)
  result = {}
  i = 0
  args.each do |arg|
    result[arg] = i
    i += 1
  end
  result
end

OPS = iota(%i[PUSH PLUS MINUS DUMP COUNT]).freeze

def simulate_program(program)
  stack = []
  program.each do |op|
    assert("Exhaustive handling of operations in simulate_program") { OPS[:COUNT] == 4 }
    case op[0]
    when :PUSH
      stack.push(op[1])
    when :PLUS
      b = stack.pop
      a = stack.pop
      stack.push(a + b)
    when :MINUS
      b = stack.pop
      a = stack.pop
      stack.push(a - b)
    when :DUMP
      puts stack.pop
    else
      assert("Unreachable") { false }
    end
  end
end

def compile_program(program, out_file_path)
  out = File.open(out_file_path, "w")
  out.write <<~HEADER
    @stack = global [1000 x i64] undef
    @sp = global i64 undef

    define void @push(i64 %val) {
      %sp = load i64, i64* @sp
      %addr = getelementptr [1000 x i64], [1000 x i64]* @stack, i64 0, i64 %sp

      store i64 %val, i64* %addr

      %newsp = add i64 %sp, 1
      store i64 %newsp, i64* @sp

      ret void
    }

    define i64 @peek() {
      %sp = load i64, i64* @sp
      %topsp = sub i64 %sp, 1
      %addr = getelementptr [1000 x i64], [1000 x i64]* @stack, i64 0, i64 %topsp
      %val = load i64, i64* %addr

      ret i64 %val
    }

    define i64 @pop() {
      %val = call i64 @peek()

      %sp = load i64, i64* @sp
      %newsp = sub i64 %sp, 1
      store i64 %newsp, i64* @sp

      ret i64 %val
    }

    declare i64 @printf(i8*, ...)

    @dump = private unnamed_addr constant [5 x i8] c"%ld\\0A\\00"

    define i32 @main() {
  HEADER

  reg = 1

  program.each do |op|
    case op[0]
    when :PUSH
      out.write "call void @push(i64 #{op[1]})\n"
    when :PLUS
      out.write "%#{reg} = call i64 @pop()\n"
      reg += 1
      out.write "%#{reg} = call i64 @pop()\n"
      reg += 1
      out.write "%#{reg} = add i64 %#{reg - 2}, %#{reg - 1}\n"
      reg += 1
      out.write "call void @push(i64 %#{reg - 1})\n"
    end
  end

  out.write <<~FOOTER
      ret i32 0
    }
  FOOTER
end

PROGRAM = [
  [:PUSH, 34],
  [:PUSH, 35],
  [:PLUS],
  [:DUMP],
  [:PUSH, 500],
  [:PUSH, 80],
  [:MINUS],
  [:DUMP],
].freeze

def usage
  puts <<~USAGE
    Usage: #{$PROGRAM_NAME} <SUBCOMMAND> [ARGS]
      SUBCOMMANDS:
        sim    Simulate the program
        com    Compile the program
  USAGE
end

if ARGV.empty?
  usage
  warn "ERROR: no subcommand is provided"
  exit 1
end

subcommand = ARGV.shift

case subcommand
when "sim"
  simulate_program(PROGRAM)
when "com"
  compile_program(PROGRAM, "output.ll")
  Process.wait Process.spawn("clang", "-v", "-O3", "output.ll", "-o", "output")
  exit $CHILD_STATUS.to_i if $CHILD_STATUS != 0
else
  usage
  warn "ERROR: unknown subcommand #{subcommand}"
  exit 1
end
