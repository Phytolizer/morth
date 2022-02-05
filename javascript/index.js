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

const program = [
    Op(OpCode.PUSH, 34),
    Op(OpCode.PUSH, 35),
    Op(OpCode.PLUS),
    Op(OpCode.DUMP),
];

simulateProgram(program);

const args = process.argv.slice(2);
