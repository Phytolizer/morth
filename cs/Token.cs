namespace Morth;

public class Token
{
    public string Text { get; }
    public string FilePath { get; }
    public int Line { get; }
    public int Column { get; }

    public Token(string text, string filePath, int line, int column)
    {
        Text = text;
        FilePath = filePath;
        Line = line;
        Column = column;
    }

    public string Location()
    {
        return $"{FilePath}:{Line}:{Column}";
    }

    public override string ToString()
    {
        return $"{Location()}: '{Text}'";
    }
}
