using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Morth;

public static class Compiler
{
    private static string ExeSuffix()
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            return ".exe";
        }

        return "";
    }

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

        return File.ReadAllText(Path.Join(temp, "build", "compiler.txt"));
    }

    private static void GenerateBinaryStackOperation(TextWriter fp, char op)
    {
        fp.WriteLine("    {");
        fp.WriteLine("        try_uint64_t b = stack_pop();");
        fp.WriteLine("        if (b.error != 0) {");
        fp.WriteLine("            print_error(b.error);");
        fp.WriteLine("            return 1;");
        fp.WriteLine("        }");
        fp.WriteLine("        try_uint64_t a = stack_pop();");
        fp.WriteLine("        if (a.error != 0) {");
        fp.WriteLine("            print_error(a.error);");
        fp.WriteLine("            return 1;");
        fp.WriteLine("        }");
        fp.WriteLine($"        int err = stack_push(a.value {op} b.value);");
        fp.WriteLine("        if (err != 0) {");
        fp.WriteLine("            print_error(err);");
        fp.WriteLine("            return 1;");
        fp.WriteLine("        }");
        fp.WriteLine("    }");
    }

    public static void CompileProgram(IEnumerable<Op> program)
    {
        using (var fp = File.CreateText("output.c"))
        {
            fp.WriteLine("#include <errno.h>");
            fp.WriteLine("#include <inttypes.h>");
            fp.WriteLine("#include <stddef.h>");
            fp.WriteLine("#include <stdint.h>");
            fp.WriteLine("#include <stdio.h>");
            fp.WriteLine("#include <stdlib.h>");
            fp.WriteLine("#include <string.h>");
            fp.WriteLine("#define ERRBUF_SIZE 64");
            fp.WriteLine("#ifdef _WIN32");
            fp.WriteLine("#define STRERROR_R(error, buffer, size) strerror_s(buffer, size, error)");
            fp.WriteLine("#else");
            fp.WriteLine("#define STRERROR_R(error, buffer, size) strerror_r(error, buffer, size)");
            fp.WriteLine("#endif");
            fp.WriteLine("typedef struct {");
            fp.WriteLine("    uint64_t* data;");
            fp.WriteLine("    size_t length;");
            fp.WriteLine("    size_t capacity;");
            fp.WriteLine("} stack_t;");
            fp.WriteLine("static stack_t stack;");
            fp.WriteLine("#define ERR_STACK_UNDERFLOW 0x8000");
            fp.WriteLine("static void print_error(int error) {");
            fp.WriteLine("    switch (error) {");
            fp.WriteLine("        case ERR_STACK_UNDERFLOW:");
            fp.WriteLine("            fprintf(stderr, \"stack underflow\\n\");");
            fp.WriteLine("            break;");
            fp.WriteLine("        default: {");
            fp.WriteLine("            char errbuf[ERRBUF_SIZE];");
            fp.WriteLine("            STRERROR_R(error, errbuf, sizeof errbuf);");
            fp.WriteLine("            fprintf(stderr, \"%s\\n\", errbuf);");
            fp.WriteLine("        } break;");
            fp.WriteLine("    }");
            fp.WriteLine("}");
            fp.WriteLine("static int stack_push(uint64_t value) {");
            fp.WriteLine("    if (stack.length == stack.capacity) {");
            fp.WriteLine("        stack.capacity = stack.capacity * 2 + 1;");
            fp.WriteLine("        uint64_t* new_stack = realloc(stack.data, sizeof(uint64_t) * stack.capacity);");
            fp.WriteLine("        if (new_stack == NULL) {");
            fp.WriteLine("            free(stack.data);");
            fp.WriteLine("            stack.data = NULL;");
            fp.WriteLine("            stack.length = 0;");
            fp.WriteLine("            stack.capacity = 0;");
            fp.WriteLine("            return ENOMEM;");
            fp.WriteLine("        }");
            fp.WriteLine("        stack.data = new_stack;");
            fp.WriteLine("    }");
            fp.WriteLine("    stack.data[stack.length] = value;");
            fp.WriteLine("    stack.length += 1;");
            fp.WriteLine("    return 0;");
            fp.WriteLine("}");
            fp.WriteLine("typedef struct { int error; uint64_t value; } try_uint64_t;");
            fp.WriteLine("static try_uint64_t stack_pop(void) {");
            fp.WriteLine("    if (stack.length == 0) {");
            fp.WriteLine("        return (try_uint64_t){.error = ERR_STACK_UNDERFLOW};");
            fp.WriteLine("    }");
            fp.WriteLine("    stack.length -= 1;");
            fp.WriteLine("    return (try_uint64_t){.value = stack.data[stack.length]};");
            fp.WriteLine("}");
            fp.WriteLine("int main(void) {");
            foreach (var op in program)
            {
                switch (op.Code)
                {
                    case OpCode.Push:
                        fp.WriteLine("    {");
                        fp.WriteLine($"        int err = stack_push({op.Value});");
                        fp.WriteLine("        if (err != 0) {");
                        fp.WriteLine("            print_error(err);");
                        fp.WriteLine("            return 1;");
                        fp.WriteLine("        }");
                        fp.WriteLine("    }");
                        break;
                    case OpCode.Plus:
                        GenerateBinaryStackOperation(fp, '+');
                        break;
                    case OpCode.Minus:
                        GenerateBinaryStackOperation(fp, '-');
                        break;
                    case OpCode.Dump:
                        fp.WriteLine("    {");
                        fp.WriteLine("        try_uint64_t value = stack_pop();");
                        fp.WriteLine("        if (value.error != 0) {");
                        fp.WriteLine("            print_error(value.error);");
                        fp.WriteLine("            return 1;");
                        fp.WriteLine("        }");
                        fp.WriteLine("        printf(\"%\" PRIu64 \"\\n\", value.value);");
                        fp.WriteLine("    }");
                        break;
                }
            }
            fp.WriteLine("    free(stack.data);");
            fp.WriteLine("}");
        }

        Subcommand.Run(FindCCompiler(), "-O2", "output.c", "-o", $"output{ExeSuffix()}");
    }
}
