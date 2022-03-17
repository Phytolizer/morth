using System.Text;
using Morth;
using MorthUtils;

namespace MorthTestRecorder;

internal static class Program
{
    private static void Main(string[] args)
    {
        foreach (var path in Directory.EnumerateFiles(Path.Join(ProjectSourcePath.Value, "..", "tests"), "*.morth"))
        {
            var program = Lexer
                .LexFile(path)
                .Select(tok => Parser.ParseTokenAsOp(tok))
                .ToArray();
            SemanticAnalyzer.CrossReferenceBlocks(program);
            var simulationOutput = new StringWriter();
            Simulator.SimulateProgram(program, simulationOutput, false);
            var outputPath = Path.ChangeExtension(path, "output");
            File.WriteAllText(outputPath, simulationOutput.ToString());
        }
    }
}
