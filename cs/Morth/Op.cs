namespace Morth;

public class Op
{
    public OpCode Code { get; }
    public SourceLocation Location { get; }
    public ulong Value { get; set; } = 0;

    private Op(OpCode code, SourceLocation location)
    {
        Code = code;
        Location = location;
    }

    private Op(OpCode code, ulong value, SourceLocation location)
    {
        Code = code;
        Value = value;
        Location = location;
    }

    public static Op Push(ulong value, SourceLocation location)
    {
        return new Op(OpCode.Push, value, location);
    }

    public static Op Plus(SourceLocation location)
    {
        return new Op(OpCode.Plus, location);
    }

    public static Op Minus(SourceLocation location)
    {
        return new Op(OpCode.Minus, location);
    }

    public static Op Equal(SourceLocation location)
    {
        return new Op(OpCode.Equal, location);
    }

    public static Op If(SourceLocation location)
    {
        return new Op(OpCode.If, location);
    }

    public static Op Else(SourceLocation location)
    {
        return new Op(OpCode.Else, location);
    }

    public static Op End(SourceLocation location)
    {
        return new Op(OpCode.End, location);
    }

    public static Op Dup(SourceLocation location)
    {
        return new Op(OpCode.Dup, location);
    }

    public static Op Greater(SourceLocation location)
    {
        return new Op(OpCode.Greater, location);
    }

    public static Op While(SourceLocation location)
    {
        return new Op(OpCode.While, location);
    }

    public static Op Do(SourceLocation location)
    {
        return new Op(OpCode.Do, location);
    }

    public static Op Mem(SourceLocation location)
    {
        return new Op(OpCode.Mem, location);
    }

    public static Op Load(SourceLocation location)
    {
        return new Op(OpCode.Load, location);
    }

    public static Op Store(SourceLocation location)
    {
        return new Op(OpCode.Store, location);
    }

    public static Op Syscall3(SourceLocation location)
    {
        return new Op(OpCode.Syscall3, location);
    }

    public static Op Dup2(SourceLocation location)
    {
        return new Op(OpCode.Dup2, location);
    }

    public static Op Less(SourceLocation location)
    {
        return new Op(OpCode.Less, location);
    }

    public static Op Swap(SourceLocation location)
    {
        return new Op(OpCode.Swap, location);
    }

    public static Op Drop(SourceLocation location)
    {
        return new Op(OpCode.Drop, location);
    }

    public static Op ShiftRight(SourceLocation location)
    {
        return new Op(OpCode.ShiftRight, location);
    }

    public static Op ShiftLeft(SourceLocation location)
    {
        return new Op(OpCode.ShiftLeft, location);
    }

    public static Op BitwiseOr(SourceLocation location)
    {
        return new Op(OpCode.BitwiseOr, location);
    }

    public static Op BitwiseAnd(SourceLocation location)
    {
        return new Op(OpCode.BitwiseAnd, location);
    }

    public static Op Over(SourceLocation location)
    {
        return new Op(OpCode.Over, location);
    }

    public static Op Modulo(SourceLocation location)
    {
        return new Op(OpCode.Modulo, location);
    }

    public static Op GreaterEqual(SourceLocation location)
    {
        return new Op(OpCode.GreaterEqual, location);
    }

    public static Op LessEqual(SourceLocation location)
    {
        return new Op(OpCode.LessEqual, location);
    }

    public static Op NotEqual(SourceLocation location)
    {
        return new Op(OpCode.NotEqual, location);
    }

    public static Op Dump(SourceLocation location)
    {
        return new Op(OpCode.Dump, location);
    }
}
