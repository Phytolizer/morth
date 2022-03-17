using System.Diagnostics;

namespace Morth;

public static class SemanticAnalyzer
{
    public static void CrossReferenceBlocks(IEnumerable<Op> programEnumerable)
    {
        var program = programEnumerable.ToArray();
        var stack = new Stack<int>();

        for (var i = 0; i < program.Length; i++)
        {
            var op = program[i];
            switch (op.Code)
            {
                case OpCode.If:
                    stack.Push(i);
                    break;
                case OpCode.Else:
                    {
                        var ifIp = stack.Pop();
                        if (program[ifIp].Code != OpCode.If)
                        {
                            throw new CompileError(op.Location, "'else' can only be used in 'if' blocks");
                        }
                        program[ifIp].Value = (ulong)(i + 1);
                        stack.Push(i);
                    }
                    break;
                case OpCode.While:
                    stack.Push(i);
                    break;
                case OpCode.Do:
                    {
                        var whileIp = stack.Pop();
                        program[i].Value = (ulong)whileIp;
                        stack.Push(i);
                    }
                    break;
                case OpCode.End:
                    {
                        var blockIp = stack.Pop();
                        if (program[blockIp].Code == OpCode.If || program[blockIp].Code == OpCode.Else)
                        {
                            program[blockIp].Value = (ulong)i;
                            program[i].Value = (ulong)(i + 1);
                        }
                        else if (program[blockIp].Code == OpCode.Do)
                        {
                            program[i].Value = program[blockIp].Value;
                            program[blockIp].Value = (ulong)(i + 1);
                        }
                        else
                        {
                            throw new CompileError(op.Location, "'end' can only close 'if'/'else' or 'do'-blocks");
                        }
                    }
                    break;
            }
        }

        if (stack.Count > 0)
        {
            throw new CompileError(program[stack.Pop()].Location, "unclosed block");
        }
    }
}
