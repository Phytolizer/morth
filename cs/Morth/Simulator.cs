namespace Morth;

public static class Simulator
{
    public static void SimulateProgram(IEnumerable<Op> programEnumerable, TextWriter output)
    {
        var stack = new Stack<ulong>();
        var program = programEnumerable.ToArray();

        for (int ip = 0; ip < program.Length;)
        {
            Op op = program[ip];
            switch (op.Code)
            {
                case OpCode.Push:
                    stack.Push(op.Value);
                    ip++;
                    break;
                case OpCode.Plus:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(a + b);
                        ip++;
                    }
                    break;
                case OpCode.Minus:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push(a - b);
                        ip++;
                    }
                    break;
                case OpCode.Equal:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push((ulong)(a == b ? 1 : 0));
                        ip++;
                    }
                    break;
                case OpCode.If:
                    {
                        var value = stack.Pop();
                        if (value == 0)
                        {
                            ip = (int)op.Value;
                        }
                        else
                        {
                            ip++;
                        }
                    }
                    break;
                case OpCode.Else:
                    ip = (int)op.Value;
                    break;
                case OpCode.End:
                    ip = (int)op.Value;
                    break;
                case OpCode.While:
                    ip++;
                    break;
                case OpCode.Do:
                    {
                        var value = stack.Pop();
                        if (value == 0)
                        {
                            ip = (int)op.Value;
                        }
                        else
                        {
                            ip++;
                        }
                    }
                    break;
                case OpCode.Dup:
                    {
                        var a = stack.Pop();
                        stack.Push(a);
                        stack.Push(a);
                        ip++;
                    }
                    break;
                case OpCode.Greater:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        stack.Push((ulong)(a > b ? 1 : 0));
                        ip++;
                    }
                    break;
                case OpCode.Mem:
                    throw new NotImplemented();
                case OpCode.Dump:
                    {
                        var value = stack.Pop();
                        output.WriteLine(value);
                    }
                    ip++;
                    break;
            }
        }
    }
}
