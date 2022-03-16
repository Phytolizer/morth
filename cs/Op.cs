namespace Morth;

public class Op
{
    public OpCode Code { get; }
    public ulong Value { get; set; } = 0;

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

    public static Op Equal()
    {
        return new Op(OpCode.Equal);
    }

    public static Op If()
    {
        return new Op(OpCode.If);
    }

    public static Op Else()
    {
        return new Op(OpCode.Else);
    }

    public static Op End()
    {
        return new Op(OpCode.End);
    }

    public static Op Dump()
    {
        return new Op(OpCode.Dump);
    }
}
