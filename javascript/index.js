import * as fs from "fs";

const OpCode = Object.freeze({
    PUSH: Symbol("PUSH"),
    PLUS: Symbol("PLUS"),
    DUMP: Symbol("DUMP"),
});

const Op = function (code, arg) {
    return {
        code: code,
        arg: arg,
    };
};

const simulateProgram = function (program) {
    const stack = [];
    for (let ip = 0; ip < program.length; ) {
        switch (program[ip].code) {
            case OpCode.PUSH:
                stack.push(program[ip].arg);
                ip += 1;
                break;
            case OpCode.PLUS:
                const b = stack.pop();
                const a = stack.pop();
                stack.push(a + b);
                ip += 1;
                break;
            case OpCode.DUMP:
                const x = stack.pop();
                console.log(x);
                ip += 1;
                break;
        }
    }
};

const compileProgram = function (program, outFilePath) {
    const out = fs.openSync(outFilePath, "w");
    for (const op of program) {
        switch (op.code) {
            case OpCode.PUSH:
                fs.writeSync(out, `push ${op.arg}\n`);
                break;
            case OpCode.PLUS:
                fs.writeSync(out, "pop rbx\n");
                fs.writeSync(out, "pop rax\n");
                fs.writeSync(out, "add rax, rbx\n");
                fs.writeSync(out, "push rax\n");
                break;
            //            case OpCode.MINUS:
            //                fs.writeSync(out, "pop rbx\n");
            //                fs.writeSync(out, "pop rax\n");
            //                fs.writeSync(out, "add rax, rbx\n");
            //                fs.writeSync(out, "push rax\n");
            //                break;
            case OpCode.DUMP:
                fs.writeSync(out, "pop rdi\n");
                fs.writeSync(out, "call dump\n");
                break;
        }
    }
};

const program = [
    Op(OpCode.PUSH, 34),
    Op(OpCode.PUSH, 35),
    Op(OpCode.PLUS),
    Op(OpCode.DUMP),
];

simulateProgram(program);
compileProgram(program, "output.asm");

const args = process.argv.slice(2);
