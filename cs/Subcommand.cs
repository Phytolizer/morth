using System.Diagnostics;

namespace Morth;

public static class Subcommand
{
    public static void Run(params string[] args)
    {
        using var process = new Process();
        process.StartInfo = new ProcessStartInfo
        {
            FileName = args[0],
            Arguments = string.Join(' ', args[1..]),
            UseShellExecute = false,
        };
        process.Start();
        process.WaitForExit();
        if (process.ExitCode != 0)
        {
            throw new SubcommandException(process.ExitCode);
        }
    }
}
