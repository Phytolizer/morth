using Morth;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Xunit;

namespace MorthTest;

public class TestRunner
{
    private static IEnumerable<object[]> GetTestRunnerData()
    {
        foreach (var file in Directory.EnumerateFiles(Path.Join(ProjectSourcePath.Value, "tests")))
        {
            yield return new object[] { file };
        }
    }

    [Theory]
    [MemberData(nameof(GetTestRunnerData))]
    public void CheckFileOutput(string path)
    {
        Console.WriteLine(path);
        var program = Lexer
            .LexFile(path)
            .Select(tok => Parser.ParseTokenAsOp(tok))
            .ToArray();
        SemanticAnalyzer.CrossReferenceBlocks(program);
        var simulationOutput = new StringWriter();
        Simulator.SimulateProgram(program, simulationOutput);
        var exePath = Compiler.CompileProgram(program, path, "temp/");
        var compilationOutput = Subcommand.RunCaptured(exePath);

        Assert.Equal(simulationOutput.ToString().ReplaceLineEndings(), compilationOutput);
    }
}