namespace Morth;

public class Op
{
    public OpCode Code { get; }
    public ulong Value { get; } = 0;

    private Op(OpCode code)
    {
        Code = code;
    }

    private Op(OpCode code, ulong value)
    {
        Code = code;
        Value = value;
    }

    public static Op Push(ulong value)
    {
        return new Op(OpCode.Push, value);
    }

    public static Op Plus()
    {
        return new Op(OpCode.Plus);
    }

    public static Op Minus()
    {
        return new Op(OpCode.Minus);
    }

    public static Op Dump()
    {
        return new Op(OpCode.Dump);
    }
}
