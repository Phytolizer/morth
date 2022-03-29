using System.Diagnostics;

namespace Morth;

public static class Compiler
{
    private static string FindCCompiler()
    {
        var temp = Path.Join(Path.GetTempPath(), "morth-cmake-temp");
        Directory.CreateDirectory(temp);

        using (var cml = File.CreateText(Path.Join(temp, "CMakeLists.txt")))
        {
            cml.WriteLine("cmake_minimum_required(VERSION 3.0)");
            cml.WriteLine("project(temp LANGUAGES C)");
            cml.WriteLine("configure_file(\"compiler.cin\" \"compiler.txt\")");
        }

        using (var compiler_cin = File.CreateText(Path.Join(temp, "compiler.cin")))
        {
            compiler_cin.WriteLine("@CMAKE_C_COMPILER@");
        }

        using (var process = new Process())
        {
            process.StartInfo = new ProcessStartInfo
            {
                FileName = "cmake",
                Arguments = "-G Ninja -B build -Wno-dev",
                WorkingDirectory = temp,
            };
            process.Start();
            process.WaitForExit();
            if (process.ExitCode != 0)
            {
                throw new SubcommandException(process.ExitCode);
            }
        }

        return File.ReadAllText(Path.Join(temp, "build", "compiler.txt")).Trim();
    }

    private static void GeneratePop(CEmitter em, string name)
    {
        em.Emit($"try_uint64_t {name} = stack_pop();");
        em.Emit($"if ({name}.error != 0) {{");
        em.AddIndent();
        em.Emit($"print_error({name}.error);");
        em.Emit("return 1;");
        em.RemoveIndent();
        em.Emit("}");
    }

    private static void GeneratePush(CEmitter em, string expression)
    {
        em.Emit("{");
        em.AddIndent();
        em.Emit($"int err = stack_push({expression});");
        em.Emit("if (err != 0) {");
        em.AddIndent();
        em.Emit("print_error(err);");
        em.Emit("return 1;");
        em.RemoveIndent();
        em.Emit("}");
        em.RemoveIndent();
        em.Emit("}");
    }

    private static void GenerateBinaryStackOperation(CEmitter em, string op)
    {
        em.Emit("{");
        em.AddIndent();
        GeneratePop(em, "b");
        GeneratePop(em, "a");
        GeneratePush(em, $"a.value {op} b.value");
        em.RemoveIndent();
        em.Emit("}");
    }

    public static string CompileProgram(IEnumerable<Op> programEnumerable, string inputPath, string outputPath)
    {
        string cPath;
        string exePath;
        if (Path.EndsInDirectorySeparator(outputPath))
        {
            Directory.CreateDirectory(outputPath);
            cPath = Path.Join(outputPath, Path.ChangeExtension(Path.GetFileName(inputPath), "c"));
            var exeSuffix = Environment.ExeSuffix();
            var baseName = Path.ChangeExtension(Path.GetFileName(inputPath), exeSuffix);
            if (exeSuffix == "")
            {
                baseName = baseName[..(baseName.Length - 1)];
            }
            exePath = Path.Join(outputPath, baseName);
        }
        else
        {
            var outputDir = Path.GetDirectoryName(outputPath);
            if (!string.IsNullOrEmpty(outputDir))
            {
                Directory.CreateDirectory(outputDir);
            }
            cPath = Path.ChangeExtension(outputPath, "c");
            var exeSuffix = Environment.ExeSuffix();
            exePath = Path.ChangeExtension(outputPath, Environment.ExeSuffix());
            if (exeSuffix == "")
            {
                exePath = exePath[..(exePath.Length - 1)];
            }
        }
        var program = programEnumerable.ToArray();
        using (var fp = File.CreateText(cPath))
        {
            var em = new CEmitter(fp);
            em.Emit("#include <errno.h>");
            em.Emit("#include <inttypes.h>");
            em.Emit("#include <stddef.h>");
            em.Emit("#include <stdint.h>");
            em.Emit("#include <stdio.h>");
            em.Emit("#include <stdlib.h>");
            em.Emit("#include <string.h>");
            em.Emit("#define ERRBUF_SIZE 64");
            em.Emit($"#define MEM_CAPACITY {Limits.MemCapacity}");
            em.Emit("#ifdef _WIN32");
            em.Emit("#define STRERROR_R(error, buffer, size) strerror_s(buffer, size, error)");
            em.Emit("#else");
            em.Emit("#define STRERROR_R(error, buffer, size) strerror_r(error, buffer, size)");
            em.Emit("#endif");
            em.Emit("typedef struct {");
            em.AddIndent();
            em.Emit("uint64_t* data;");
            em.Emit("size_t length;");
            em.Emit("size_t capacity;");
            em.RemoveIndent();
            em.Emit("} stack_t;");
            em.Emit("static stack_t stack;");
            em.Emit("static uint8_t mem[MEM_CAPACITY];");
            em.Emit("#define ERR_STACK_UNDERFLOW 0x8000");
            em.Emit("#define ERR_UNKNOWN_FD 0x8001");
            em.Emit("#define ERR_UNKNOWN_SYSCALL 0x8002");
            em.Emit("static void print_error(int error) {");
            em.AddIndent();
            em.Emit("switch (error) {");
            em.AddIndent();
            em.Emit("case ERR_STACK_UNDERFLOW:");
            em.AddIndent();
            em.Emit("fprintf(stderr, \"stack underflow\\n\");");
            em.Emit("break;");
            em.RemoveIndent();
            em.Emit("case ERR_UNKNOWN_FD:");
            em.AddIndent();
            em.Emit("fprintf(stderr, \"unknown file descriptor\\n\");");
            em.Emit("break;");
            em.RemoveIndent();
            em.Emit("case ERR_UNKNOWN_SYSCALL:");
            em.AddIndent();
            em.Emit("fprintf(stderr, \"unknown syscall\\n\");");
            em.Emit("break;");
            em.RemoveIndent();
            em.Emit("default: {");
            em.AddIndent();
            em.Emit("char errbuf[ERRBUF_SIZE];");
            em.Emit("STRERROR_R(error, errbuf, sizeof errbuf);");
            em.Emit("fprintf(stderr, \"%s\\n\", errbuf);");
            em.RemoveIndent();
            em.Emit("} break;");
            em.RemoveIndent();
            em.Emit("}");
            em.RemoveIndent();
            em.Emit("}");
            em.Emit("static int stack_push(uint64_t value) {");
            em.AddIndent();
            em.Emit("if (stack.length == stack.capacity) {");
            em.AddIndent();
            em.Emit("stack.capacity = stack.capacity * 2 + 1;");
            em.Emit("uint64_t* new_stack = realloc(stack.data, sizeof(uint64_t) * stack.capacity);");
            em.Emit("if (new_stack == NULL) {");
            em.AddIndent();
            em.Emit("free(stack.data);");
            em.Emit("stack.data = NULL;");
            em.Emit("stack.length = 0;");
            em.Emit("stack.capacity = 0;");
            em.Emit("return ENOMEM;");
            em.RemoveIndent();
            em.Emit("}");
            em.Emit("stack.data = new_stack;");
            em.RemoveIndent();
            em.Emit("}");
            em.Emit("stack.data[stack.length] = value;");
            em.Emit("stack.length += 1;");
            em.Emit("return 0;");
            em.RemoveIndent();
            em.Emit("}");
            em.Emit("typedef struct { int error; uint64_t value; } try_uint64_t;");
            em.Emit("static try_uint64_t stack_pop(void) {");
            em.AddIndent();
            em.Emit("if (stack.length == 0) {");
            em.AddIndent();
            em.Emit("return (try_uint64_t){.error = ERR_STACK_UNDERFLOW};");
            em.RemoveIndent();
            em.Emit("}");
            em.Emit("stack.length -= 1;");
            em.Emit("return (try_uint64_t){.value = stack.data[stack.length]};");
            em.RemoveIndent();
            em.Emit("}");
            em.Emit("static int syscall3(uint64_t syscall_number, uint64_t arg1, uint64_t arg2, uint64_t arg3) {");
            em.AddIndent();
            em.Emit("switch (syscall_number) {");
            em.AddIndent();
            em.Emit("case 1: {");
            em.AddIndent();
            em.Emit("uint64_t fd = arg1;");
            em.Emit("char* buf = (char*)&mem[arg2];");
            em.Emit("uint64_t count = arg3;");
            em.Emit("switch (fd) {");
            em.AddIndent();
            em.Emit("case 1:");
            em.AddIndent();
            em.Emit("printf(\"%.*s\", (int)count, buf);");
            em.Emit("break;");
            em.RemoveIndent();
            em.Emit("case 2:");
            em.AddIndent();
            em.Emit("fprintf(stderr, \"%.*s\", (int)count, buf);");
            em.Emit("break;");
            em.RemoveIndent();
            em.Emit("default:");
            em.AddIndent();
            em.Emit("return ERR_UNKNOWN_FD;");
            em.RemoveIndent();
            em.RemoveIndent();
            em.Emit("}");
            GeneratePush(em, "count");
            em.RemoveIndent();
            em.Emit("} break;");
            em.Emit("default:");
            em.AddIndent();
            em.Emit("return ERR_UNKNOWN_SYSCALL;");
            em.RemoveIndent();
            em.RemoveIndent();
            em.Emit("}");
            em.Emit("return 0;");
            em.RemoveIndent();
            em.Emit("}");
            em.Emit("int main(void) {");
            em.AddIndent();
            var ip = 0;
            for (; ip < program.Length; ip++)
            {
                Op op = program[ip];
                em.EmitNoIndent($"porth_addr_{ip}:");
                em.Emit($"// {op.Code}");
                switch (op.Code)
                {
                    case OpCode.Push:
                        GeneratePush(em, op.Value.ToString());
                        break;
                    case OpCode.Plus:
                        GenerateBinaryStackOperation(em, "+");
                        break;
                    case OpCode.Minus:
                        GenerateBinaryStackOperation(em, "-");
                        break;
                    case OpCode.Equal:
                        GenerateBinaryStackOperation(em, "==");
                        break;
                    case OpCode.If:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "value");
                        em.Emit("if (value.value == 0) {");
                        em.AddIndent();
                        em.Emit($"goto porth_addr_{op.Value};");
                        em.RemoveIndent();
                        em.Emit("}");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Else:
                        em.Emit($"goto porth_addr_{op.Value};");
                        break;
                    case OpCode.End:
                        em.Emit($"goto porth_addr_{op.Value};");
                        break;
                    case OpCode.Dup:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "value");
                        GeneratePush(em, "value.value");
                        GeneratePush(em, "value.value");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Greater:
                        GenerateBinaryStackOperation(em, ">");
                        break;
                    case OpCode.While:
                        em.Emit(";");
                        break;
                    case OpCode.Do:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "value");
                        em.Emit("if (value.value == 0) {");
                        em.AddIndent();
                        em.Emit($"goto porth_addr_{op.Value};");
                        em.RemoveIndent();
                        em.Emit("}");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Mem:
                        GeneratePush(em, "0");
                        break;
                    case OpCode.Load:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "value");
                        GeneratePush(em, "mem[value.value]");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Store:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "b");
                        GeneratePop(em, "a");
                        em.Emit("mem[a.value] = (uint8_t)b.value;");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Syscall3:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "syscall_number");
                        GeneratePop(em, "arg1");
                        GeneratePop(em, "arg2");
                        GeneratePop(em, "arg3");
                        em.Emit("int error = syscall3(syscall_number.value, arg1.value, arg2.value, arg3.value);");
                        em.Emit("if (error != 0) {");
                        em.AddIndent();
                        em.Emit("print_error(error);");
                        em.Emit("return 1;");
                        em.RemoveIndent();
                        em.Emit("}");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Dup2:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "b");
                        GeneratePop(em, "a");
                        GeneratePush(em, "a.value");
                        GeneratePush(em, "b.value");
                        GeneratePush(em, "a.value");
                        GeneratePush(em, "b.value");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Less:
                        GenerateBinaryStackOperation(em, "<");
                        break;
                    case OpCode.Swap:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "b");
                        GeneratePop(em, "a");
                        GeneratePush(em, "b.value");
                        GeneratePush(em, "a.value");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Drop:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "value");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.ShiftRight:
                        GenerateBinaryStackOperation(em, ">>");
                        break;
                    case OpCode.ShiftLeft:
                        GenerateBinaryStackOperation(em, "<<");
                        break;
                    case OpCode.BitwiseOr:
                        GenerateBinaryStackOperation(em, "|");
                        break;
                    case OpCode.BitwiseAnd:
                        GenerateBinaryStackOperation(em, "&");
                        break;
                    case OpCode.Over:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "b");
                        GeneratePop(em, "a");
                        GeneratePush(em, "a.value");
                        GeneratePush(em, "b.value");
                        GeneratePush(em, "a.value");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Modulo:
                        GenerateBinaryStackOperation(em, "%");
                        break;
                    case OpCode.GreaterEqual:
                        GenerateBinaryStackOperation(em, ">=");
                        break;
                    case OpCode.LessEqual:
                        GenerateBinaryStackOperation(em, "<=");
                        break;
                    case OpCode.NotEqual:
                        GenerateBinaryStackOperation(em, "!=");
                        break;
                    case OpCode.Dump:
                        em.Emit("{");
                        em.AddIndent();
                        GeneratePop(em, "value");
                        em.Emit("printf(\"%\" PRIu64 \"\\n\", value.value);");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                }
            }
            em.EmitNoIndent($"porth_addr_{ip}:");
            em.Emit("free(stack.data);");
            em.RemoveIndent();
            em.Emit("}");
        }

        Subcommand.Run(FindCCompiler(), "-O2", cPath, "-o", exePath);

        return exePath;
    }
}
