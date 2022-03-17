namespace Morth;

public static class Parser
{
    public static Op ParseTokenAsOp(Token token)
    {
        if (token.Text == "+")
        {
            return Op.Plus(token.Location);
        }
        if (token.Text == "-")
        {
            return Op.Minus(token.Location);
        }
        if (token.Text == "dump")
        {
            return Op.Dump(token.Location);
        }
        if (token.Text == "=")
        {
            return Op.Equal(token.Location);
        }
        if (token.Text == "if")
        {
            return Op.If(token.Location);
        }
        if (token.Text == "else")
        {
            return Op.Else(token.Location);
        }
        if (token.Text == "end")
        {
            return Op.End(token.Location);
        }
        if (token.Text == "dup")
        {
            return Op.Dup(token.Location);
        }
        if (token.Text == ">")
        {
            return Op.Greater(token.Location);
        }
        if (token.Text == "while")
        {
            return Op.While(token.Location);
        }
        if (token.Text == "do")
        {
            return Op.Do(token.Location);
        }
        if (token.Text == "mem")
        {
            return Op.Mem(token.Location);
        }
        if (token.Text == ".")
        {
            return Op.Load(token.Location);
        }
        if (token.Text == ",")
        {
            return Op.Store(token.Location);
        }

        if (!ulong.TryParse(token.Text, out var value))
        {
            throw new ParseException(token, "Unknown token.");
        }

        return Op.Push(value, token.Location);
    }
}
