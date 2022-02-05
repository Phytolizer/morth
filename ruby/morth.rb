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
              segment .text
              dump:
              sub     rsp, 40
              lea     rsi, [rsp + 31]
              mov     byte [rsp + 31], 10
              mov     ecx, 1
              mov     r8, -3689348814741910323
              .LBB0_1:
              mov     rax, rdi
              mul     r8
              shr     rdx, 3
              lea     eax, [rdx + rdx]
              lea     r9d, [rax + 4*rax]
              mov     eax, edi
              sub     eax, r9d
              or      al, 48
              mov     byte [rsi - 1], al
              add     rsi, -1
              add     rcx, 1
              cmp     rdi, 9
              mov     rdi, rdx
              ja      .LBB0_1
              mov     edi, 1
              mov     rdx, rcx
              mov     rax, 1
              syscall
              add     rsp, 40
              ret
              global _start
              _start:
            HEADER

  program.each do |op|
    case op[0]
    when :PUSH
      out.write "push #{op[1]}\n"
    when :PLUS
      out.write "pop rbx\n"
      out.write "pop rax\n"
      out.write "add rax, rbx\n"
      out.write "push rax\n"
    end
  end

  out.write <<~FOOTER
              mov rax, 60
              mov rdi, 0
              syscall
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
  compile_program(PROGRAM, "output.asm")
  Process.wait Process.spawn("nasm", "-felf64", "output.asm")
  exit $CHILD_STATUS.to_i if $CHILD_STATUS != 0
  Process.wait Process.spawn("ld", "-o", "output", "output.o")
  exit $CHILD_STATUS.to_i if $CHILD_STATUS != 0
else
  usage
  warn "ERROR: unknown subcommand #{subcommand}"
  exit 1
end
