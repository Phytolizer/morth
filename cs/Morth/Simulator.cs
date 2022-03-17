using System.Diagnostics;
using System.Text;

namespace Morth;

public static class Simulator
{
    public static void SimulateProgram(IEnumerable<Op> programEnumerable, TextWriter output)
    {
        var stack = new Stack<ulong>();
        var program = programEnumerable.ToArray();
        var mem = new byte[Limits.MemCapacity];

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
                    stack.Push(0);
                    ip++;
                    break;
                case OpCode.Load:
                    {
                        var addr = stack.Pop();
                        var b = mem[(int)addr];
                        stack.Push((ulong)b);
                        ip++;
                    }
                    break;
                case OpCode.Store:
                    {
                        var b = stack.Pop();
                        var a = stack.Pop();
                        mem[(int)a] = (byte)(b & 0xFF);
                        ip++;
                    }
                    break;
                case OpCode.Syscall3:
                    {
                        var syscallNumber = stack.Pop();
                        var arg1 = stack.Pop();
                        var arg2 = stack.Pop();
                        var arg3 = stack.Pop();
                        if (syscallNumber == 1)
                        {
                            var fd = arg1;
                            var buf = arg2;
                            var count = arg3;
                            var s = mem[(int)buf..(int)(buf + count)];
                            var str = Encoding.Default.GetString(s);
                            switch (fd)
                            {
                                case 1:
                                    Console.Write(str);
                                    break;
                                case 2:
                                    Console.Error.Write(str);
                                    break;
                                default:
                                    Debug.Fail($"unknown fd {fd}");
                                    break;
                            }
                        }
                        else
                        {
                            Debug.Fail($"unknown syscall {syscallNumber}");
                        }
                        ip++;
                    }
                    break;
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
