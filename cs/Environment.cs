using System.Runtime.InteropServices;

namespace Morth;

public static class Environment
{
    public static string ExeSuffix()
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            return ".exe";
        }

        return "";
    }
}
