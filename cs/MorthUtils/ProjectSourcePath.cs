using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace MorthUtils;

public static class ProjectSourcePath
{
    private const string _relativePath = $"{nameof(ProjectSourcePath)}.cs";
    private static string? _lazyValue;
    public static string Value => _lazyValue ??= CalculatePath();

    private static string CalculatePath()
    {
        var pathName = GetSourceFilePathName();
        Debug.Assert(pathName.EndsWith(_relativePath, StringComparison.Ordinal));
        return pathName.Substring(0, pathName.Length - _relativePath.Length);
    }

    public static string GetSourceFilePathName([CallerFilePath] string? callerFilePath = null) => callerFilePath ?? "";
}
