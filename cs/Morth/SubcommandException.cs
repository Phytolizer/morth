namespace Morth;

public class SubcommandException : Exception
{
    public SubcommandException(int code) : base($"Subprocess exited with code {code}.")
    {
    }
}
