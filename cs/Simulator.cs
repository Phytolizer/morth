namespace Morth;

public static class Simulator
{
    public static void SimulateProgram(IEnumerable<Op> program)
    {
        var stack = new Stack<ulong>();

        foreach (var op in program)
        {
            switch (op.Code)
            {
                case OpCode.Push:
                    stack.Push(op.Value);
                    break;
                case OpCode.Plus:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(a + b);
                    }
                    break;
                case OpCode.Minus:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(a - b);
                    }
                    break;
                case OpCode.Dump:
                    {
                        var value = stack.Pop();
                        Console.WriteLine(value);
                    }
                    break;
            }
        }
    }
}
