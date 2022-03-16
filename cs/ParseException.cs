namespace Morth;

internal class ParseException : Exception
{
    public ParseException(Token token, string why) : base($"{token}: {why}")
    {
    }
}
