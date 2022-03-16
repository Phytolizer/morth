using System.Diagnostics;
using System.Runtime.InteropServices;

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

    private static void GenerateBinaryStackOperation(CEmitter fp, char op)
    {
        fp.Emit("{");
        fp.AddIndent();
        fp.Emit("try_uint64_t b = stack_pop();");
        fp.Emit("if (b.error != 0) {");
        fp.AddIndent();
        fp.Emit("print_error(b.error);");
        fp.Emit("return 1;");
        fp.RemoveIndent();
        fp.Emit("}");
        fp.Emit("try_uint64_t a = stack_pop();");
        fp.Emit("if (a.error != 0) {");
        fp.AddIndent();
        fp.Emit("print_error(a.error);");
        fp.Emit("return 1;");
        fp.RemoveIndent();
        fp.Emit("}");
        fp.Emit($"int err = stack_push(a.value {op} b.value);");
        fp.Emit("if (err != 0) {");
        fp.AddIndent();
        fp.Emit("print_error(err);");
        fp.Emit("return 1;");
        fp.RemoveIndent();
        fp.Emit("}");
        fp.RemoveIndent();
        fp.Emit("}");
    }

    public static void CompileProgram(IEnumerable<Op> program)
    {
        using (var fp = File.CreateText("output.c"))
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
            em.Emit("#define ERR_STACK_UNDERFLOW 0x8000");
            em.Emit("static void print_error(int error) {");
            em.AddIndent();
            em.Emit("switch (error) {");
            em.AddIndent();
            em.Emit("case ERR_STACK_UNDERFLOW:");
            em.AddIndent();
            em.Emit("fprintf(stderr, \"stack underflow\\n\");");
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
            em.Emit("int main(void) {");
            em.AddIndent();
            foreach (var op in program)
            {
                switch (op.Code)
                {
                    case OpCode.Push:
                        em.Emit("{");
                        em.AddIndent();
                        em.Emit($"int err = stack_push({op.Value});");
                        em.Emit("if (err != 0) {");
                        em.AddIndent();
                        em.Emit("print_error(err);");
                        em.Emit("return 1;");
                        em.RemoveIndent();
                        em.Emit("}");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                    case OpCode.Plus:
                        GenerateBinaryStackOperation(em, '+');
                        break;
                    case OpCode.Minus:
                        GenerateBinaryStackOperation(em, '-');
                        break;
                    case OpCode.Dump:
                        em.Emit("{");
                        em.AddIndent();
                        em.Emit("try_uint64_t value = stack_pop();");
                        em.Emit("if (value.error != 0) {");
                        em.AddIndent();
                        em.Emit("print_error(value.error);");
                        em.Emit("return 1;");
                        em.RemoveIndent();
                        em.Emit("}");
                        em.Emit("printf(\"%\" PRIu64 \"\\n\", value.value);");
                        em.RemoveIndent();
                        em.Emit("}");
                        break;
                }
            }
            em.Emit("free(stack.data);");
            em.RemoveIndent();
            em.Emit("}");
        }

        Subcommand.Run(FindCCompiler(), "-O2", "output.c", "-o", $"output{Environment.ExeSuffix()}");
    }
}
