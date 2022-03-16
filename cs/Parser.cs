namespace Morth;

public static class Parser
{
    public static Op ParseTokenAsOp(Token token)
    {
        if (token.Text == "+")
        {
            return Op.Plus();
        }
        if (token.Text == "-")
        {
            return Op.Minus();
        }
        if (token.Text == ".")
        {
            return Op.Dump();
        }
        if (token.Text == "=")
        {
            return Op.Equal();
        }

        if (!ulong.TryParse(token.Text, out var value))
        {
            throw new ParseException(token, "Unknown token.");
        }

        return Op.Push(value);
    }
}
