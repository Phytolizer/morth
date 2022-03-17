namespace Morth;

public class SubcommandException : Exception
{
    public int ExitCode { get; }

    public SubcommandException(int code) : base($"Subprocess exited with code {code}.")
    {
        ExitCode = code;
    }
}
