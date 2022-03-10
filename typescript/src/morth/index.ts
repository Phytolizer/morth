export {};

enum OpCode {
    Push,
    Plus,
    Minus,
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

    static minus() {
        return new Op(OpCode.Minus);
    }

    static dump() {
        return new Op(OpCode.Dump);
    }
}

function simulateProgram(program: Op[]) {
    const stack: number[] = [];
    for (let ip = 0; ip < program.length; ) {
        switch (program[ip].code) {
            case OpCode.Push:
                stack.push(program[ip].value!);
                ip += 1;
                break;
            case OpCode.Plus: {
                const b: number = stack.pop()!;
                const a: number = stack.pop()!;
                stack.push(a + b);
                ip += 1;
                break;
            }
            case OpCode.Minus: {
                const b: number = stack.pop()!;
                const a: number = stack.pop()!;
                stack.push(a - b);
                ip += 1;
                break;
            }
            case OpCode.Dump:
                console.log(stack.pop()!);
                ip += 1;
                break;
        }
    }
}

function compileProgram(program: Op[]) {}

const program: Op[] = [
    Op.push(34),
    Op.push(35),
    Op.plus(),
    Op.dump(),
    Op.push(500),
    Op.push(80),
    Op.minus(),
    Op.dump(),
];

const args: string[] = process.argv.slice(2);

function usage() {
    console.log("Usage: porth <SUBCOMMAND> [ARGS]");
    console.log("SUBCOMMANDS:");
    console.log("  sim              Simulate the program");
    console.log("  com              Compile the program");
}

if (args.length == 0) {
    usage();
    console.log("ERROR: no subcommand is provided");
    process.exit(1);
}

switch (args[0]) {
    case "sim":
        simulateProgram(program);
        break;
    case "com":
        compileProgram(program);
        break;
    default:
        usage();
        console.log(`ERROR: unknown subcommand ${args[0]}`);
        process.exit(1);
}
