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
        if (token.Text == "if")
        {
            return Op.If();
        }
        if (token.Text == "else")
        {
            return Op.Else();
        }
        if (token.Text == "end")
        {
            return Op.End();
        }
        if (token.Text == "dup")
        {
            return Op.Dup();
        }
        if (token.Text == ">")
        {
            return Op.Greater();
        }
        if (token.Text == "while")
        {
            return Op.While();
        }
        if (token.Text == "do")
        {
            return Op.Do();
        }

        if (!ulong.TryParse(token.Text, out var value))
        {
            throw new ParseException(token, "Unknown token.");
        }

        return Op.Push(value);
    }
}
