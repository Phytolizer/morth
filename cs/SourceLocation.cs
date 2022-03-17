namespace Morth;

public record SourceLocation
{
    public string FilePath { get; }
    public int Row { get; }
    public int Column { get; }

    public SourceLocation(string filePath, int row, int column)
    {
        FilePath = filePath;
        Row = row;
        Column = column;
    }

    public override string ToString()
    {
        return $"{FilePath}:{Row}:{Column}";
    }
}
