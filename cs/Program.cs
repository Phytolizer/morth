namespace Morth;

internal static class Program
{
    private static List<Op> TestProgram { get; } = new List<Op>
    {
        Op.Push(34),
        Op.Push(35),
        Op.Plus(),
        Op.Dump(),
    };

    private static void Main(string[] args)
    {
        Compiler.CompileProgram(TestProgram);
    }
}
