using CommandLine;
using Morth;

internal static class Program
{
    public class Options
    {
        [Option("debug", Required = false, HelpText = "Enable debug mode.")]
        public bool Debug { get; set; }
    }

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
        [Option('o', "output", Required = false, HelpText = "Set the output file path")]
        public string OutputPath { get; set; } = "output";
    }

    private static void Main(string[] args)
    {
        var debug = false;
        try
        {
            CommandLine.Parser.Default.ParseArguments<Options, CompileOptions, SimulationOptions>(args)
                .WithParsed<Options>(o =>
                {
                    debug = o.Debug;
                    if (debug)
                    {
                        Console.Error.WriteLine("[INFO] Debug mode is enabled");
                    }
                })
                .WithParsed<SimulationOptions>(o =>
                {
                    var program = Lexer
                        .LexFile(o.InputPath)
                        .Select(tok => Morth.Parser.ParseTokenAsOp(tok))
                        .ToArray();
                    SemanticAnalyzer.CrossReferenceBlocks(program);
                    Simulator.SimulateProgram(program, Console.Out, debug);
                })
                .WithParsed<CompileOptions>(o =>
                {
                    var program = Lexer
                        .LexFile(o.InputPath)
                        .Select(tok => Morth.Parser.ParseTokenAsOp(tok))
                        .ToArray();
                    SemanticAnalyzer.CrossReferenceBlocks(program);
                    var exePath = Compiler.CompileProgram(program, o.InputPath, o.OutputPath);
                    if (o.Run)
                    {
                        try
                        {
                            Subcommand.Run(Path.Join(System.Environment.CurrentDirectory, exePath));
                        }
                        catch (SubcommandException e)
                        {
                            System.Environment.ExitCode = e.ExitCode;
                        }
                    }
                });
        }
        catch (CompileError error)
        {
            Console.Error.WriteLine(error);
            System.Environment.ExitCode = 1;
        }
    }
}
