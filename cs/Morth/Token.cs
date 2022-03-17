namespace Morth;

public class Token
{
    public string Text { get; }
    public SourceLocation Location { get; }

    public Token(string text, SourceLocation location)
    {
        Text = text;
        Location = location;
    }

    public override string ToString()
    {
        return $"{Location}: '{Text}'";
    }
}
