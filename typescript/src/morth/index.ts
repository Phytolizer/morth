import * as fs from "fs";

enum OpCode {
    Push,
    Plus,
    Dump,
}

class Op {
    constructor(public code: OpCode, public value?: number) {}

    static push(value: number) {
        return new Op(OpCode.Push, value);
    }

    static plus() {
        return new Op(OpCode.Plus);
    }

    static dump() {
        return new Op(OpCode.Dump);
    }
}

const simulateProgram = function (program: Op[]) {
    const stack: number[] = [];
    for (let ip = 0; ip < program.length; ) {
        switch (program[ip].code) {
            case OpCode.Push:
                stack.push(program[ip].value!);
                ip += 1;
                break;
            case OpCode.Plus:
                const b: number = stack.pop()!;
                const a: number = stack.pop()!;
                stack.push(a + b);
                ip += 1;
                break;
            case OpCode.Dump:
                console.log(stack.pop()!);
                ip += 1;
                break;
        }
    }
};

const program = [Op.push(34), Op.push(35), Op.plus(), Op.dump()];

simulateProgram(program);
