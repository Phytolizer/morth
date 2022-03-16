using CommandLine;

namespace Morth;

internal static class Program
{
    [Verb("sim", HelpText = "Simulate the program.")]
    public class SimulationOptions
    {
    }

    [Verb("com", HelpText = "Compile the program.")]
    public class CompileOptions
    {
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
        Parser.Default.ParseArguments<CompileOptions, SimulationOptions>(args[1..])
        .WithParsed<SimulationOptions>(o =>
        {
            Simulator.SimulateProgram(TestProgram);
        })
        .WithParsed<CompileOptions>(o =>
        {
            Compiler.CompileProgram(TestProgram);
            if (o.Run)
            {
                Subcommand.Run($"./output{MyEnvironment.ExeSuffix()}");
            }
        });
    }
}
