#[derive(Debug, PartialEq)]
enum Op {
    Push(u64),
    Plus,
    Minus,
    Dump,
}

fn simulate_program(program: &[Op]) {
    let mut stack = Vec::<u64>::new();
    let mut ip = 0;
    while ip < program.len() {
        println!("Executing [{}] {:?}", ip, program[ip]);
        match &program[ip] {
            Op::Push(x) => {
                stack.push(*x);
                ip += 1;
            }
            Op::Plus => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a + b);
                ip += 1;
            }
            Op::Minus => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a - b);
                ip += 1;
            }
            Op::Dump => {
                let x = stack.pop().unwrap();
                println!("{}", x);
                ip += 1;
            }
        }
    }
}

fn compile_program(program: &[Op]) {}

fn main() {
    simulate_program(&[
        Op::Push(34),
        Op::Push(35),
        Op::Plus,
        Op::Dump,
        Op::Push(500),
        Op::Push(80),
        Op::Minus,
        Op::Dump,
    ]);
}
