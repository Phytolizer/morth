using Morth;
using MorthUtils;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Xunit;
using Xunit.Abstractions;

namespace MorthTest;

public class TestRunner
{
    private readonly ITestOutputHelper _output;

    public TestRunner(ITestOutputHelper output)
    {
        _output = output;
    }

    private static IEnumerable<object[]> GetTestRunnerData()
    {
        foreach (var file in Directory.EnumerateFiles(Path.Join(ProjectSourcePath.Value, "..", "tests"), "*.morth"))
        {
            yield return new object[] { Path.GetFullPath(file) };
        }
    }

    [Theory]
    [MemberData(nameof(GetTestRunnerData))]
    public void CheckFileOutput(string path)
    {
        _output.WriteLine($"running for {path}");
        var outputTxtPath = Path.ChangeExtension(path, "output");
        var expectedOutput = File.ReadAllText(outputTxtPath);
        var program = Lexer
            .LexFile(path)
            .Select(tok => Parser.ParseTokenAsOp(tok))
            .ToArray();
        SemanticAnalyzer.CrossReferenceBlocks(program);
        var simulationOutput = new StringWriter();
        Simulator.SimulateProgram(program, simulationOutput, false);

        Assert.Equal(simulationOutput.ToString().ReplaceLineEndings(), expectedOutput);

        var exePath = Compiler.CompileProgram(program, path, "temp/");
        var compilationOutput = Subcommand.RunCaptured(exePath);

        Assert.Equal(compilationOutput.ReplaceLineEndings(), expectedOutput);
    }
}
