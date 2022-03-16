using CommandLine;

namespace Morth;

internal static class Program
{
    [Verb("sim", HelpText = "Simulate the program.")]
    public class SimulationOptions
    {
        [Value(0, Required = true, MetaName = "InputPath", HelpText = "Input filename.")]
        public string InputPath { get; set; } = "";
    }

    [Verb("com", HelpText = "Compile the program.")]
    public class CompileOptions
    {
        [Value(0, Required = true, MetaName = "InputPath", HelpText = "Input filename.")]
        public string InputPath { get; set; } = "";

        [Option('r', "run", Required = false, HelpText = "Run the compiled program immediately.")]
        public bool Run { get; set; }
    }

    private static List<Op> TestProgram { get; } = new List<Op>
    {
        Op.Push(34),
        Op.Push(35),
        Op.Plus(),
        Op.Dump(),
    };

    private static void Main(string[] args)
    {
        CommandLine.Parser.Default.ParseArguments<CompileOptions, SimulationOptions>(args[1..])
        .WithParsed<SimulationOptions>(o =>
        {
            var program = Lexer
                .LexFile(o.InputPath)
                .Select(tok => Parser.ParseTokenAsOp(tok))
                .ToArray();
            Simulator.SimulateProgram(program);
        })
        .WithParsed<CompileOptions>(o =>
        {
            var program = Lexer
                .LexFile(o.InputPath)
                .Select(tok => Parser.ParseTokenAsOp(tok))
                .ToArray();
            Compiler.CompileProgram(program);
            if (o.Run)
            {
                Subcommand.Run($"./output{MyEnvironment.ExeSuffix()}");
            }
        });
    }
}
