namespace Morth;

public class CompileError : Exception
{
    public CompileError(SourceLocation location, string message) : base($"{location}: {message}")
    {
    }
}
