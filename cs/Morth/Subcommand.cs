using System.Diagnostics;

namespace Morth;

public static class Subcommand
{
    public static void Run(params string[] args)
    {
        Console.WriteLine($"> {string.Join(' ', args)}");

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

    public static string RunCaptured(params string[] args)
    {
        using var process = new Process();
        process.StartInfo = new ProcessStartInfo
        {
            FileName = args[0],
            Arguments = string.Join(' ', args[1..]),
            UseShellExecute = false,
            RedirectStandardOutput = true,
        };
        process.Start();
        process.WaitForExit();
        if (process.ExitCode != 0)
        {
            throw new SubcommandException(process.ExitCode);
        }
        return process.StandardOutput.ReadToEnd();
    }
}
