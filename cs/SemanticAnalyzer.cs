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
                        Debug.Assert(program[ifIp].Code == OpCode.If, "'else' can only be used in 'if' blocks");
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
                            Debug.Fail("'end' can only close 'if'/'else' blocks");
                        }
                    }
                    break;
            }
        }
    }
}
