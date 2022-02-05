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

fn compile_program(program: &[Op]) {
    for op in program {}
}

const PROGRAM: &[Op] = &[
    Op::Push(34),
    Op::Push(35),
    Op::Plus,
    Op::Dump,
    Op::Push(500),
    Op::Push(80),
    Op::Minus,
    Op::Dump,
];

fn usage(argv0: &str) {
    println!("Usage: {} <SUBCOMMAND> [ARGS]", argv0);
    println!("  SUBCOMMANDS:");
    println!("    sim       Simulate the program");
    println!("    com       Compile the program");
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        usage(&args[0]);
        eprintln!("ERROR: no subcommand is provided");
        std::process::exit(1);
    }

    let subcommand = &args[1];
    match subcommand.as_str() {
        "sim" => simulate_program(PROGRAM),
        "com" => compile_program(PROGRAM),
        _ => {
            usage(&args[0]);
            eprintln!("ERROR: unknown subcommand {}", subcommand);
            std::process::exit(1);
        }
    }
}
