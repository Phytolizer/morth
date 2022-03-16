namespace Morth;

public static class Simulator
{
    public static void SimulateProgram(IEnumerable<Op> programEnumerable)
    {
        var stack = new Stack<ulong>();
        var program = programEnumerable.ToArray();

        for (int i = 0; i < program.Length;)
        {
            Op op = program[i];
            switch (op.Code)
            {
                case OpCode.Push:
                    stack.Push(op.Value);
                    i++;
                    break;
                case OpCode.Plus:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(a + b);
                        i++;
                    }
                    break;
                case OpCode.Minus:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(a - b);
                        i++;
                    }
                    break;
                case OpCode.Equal:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push((ulong)(a == b ? 1 : 0));
                        i++;
                    }
                    break;
                case OpCode.If:
                    {
                        var value = stack.Pop();
                        if (value == 0)
                        {
                            i = (int)op.Value;
                        }
                        else
                        {
                            i++;
                        }
                    }
                    break;
                case OpCode.Else:
                    i = (int)op.Value;
                    break;
                case OpCode.End:
                    i++;
                    break;
                case OpCode.Dup:
                    {
                        var a = stack.Pop();
                        stack.Push(a);
                        stack.Push(a);
                        i++;
                    }
                    break;
                case OpCode.Greater:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push((ulong)(a > b ? 1 : 0));
                        i++;
                    }
                    break;
                case OpCode.Dump:
                    {
                        var value = stack.Pop();
                        Console.WriteLine(value);
                    }
                    i++;
                    break;
            }
        }
    }
}
