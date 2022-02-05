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

fn compile_program(program: &[Op], out_file_path: &str) -> std::io::Result<()> {
    use std::io::Write;

    let mut out = std::fs::File::create(out_file_path)?;

    writeln!(out, "segment .text")?;
    writeln!(out, "dump:")?;
    writeln!(out, "sub     rsp, 40")?;
    writeln!(out, "lea     rsi, [rsp + 31]")?;
    writeln!(out, "mov     byte [rsp + 31], 10")?;
    writeln!(out, "mov     ecx, 1")?;
    writeln!(out, "mov     r8, -3689348814741910323")?;
    writeln!(out, ".LBB0_1:")?;
    writeln!(out, "mov     rax, rdi")?;
    writeln!(out, "mul     r8")?;
    writeln!(out, "shr     rdx, 3")?;
    writeln!(out, "lea     eax, [rdx + rdx]")?;
    writeln!(out, "lea     r9d, [rax + 4*rax]")?;
    writeln!(out, "mov     eax, edi")?;
    writeln!(out, "sub     eax, r9d")?;
    writeln!(out, "or      al, 48")?;
    writeln!(out, "mov     byte [rsi - 1], al")?;
    writeln!(out, "add     rsi, -1")?;
    writeln!(out, "add     rcx, 1")?;
    writeln!(out, "cmp     rdi, 9")?;
    writeln!(out, "mov     rdi, rdx")?;
    writeln!(out, "ja      .LBB0_1")?;
    writeln!(out, "mov     edi, 1")?;
    writeln!(out, "mov     rdx, rcx")?;
    writeln!(out, "mov     rax, 1")?;
    writeln!(out, "syscall")?;
    writeln!(out, "add     rsp, 40")?;
    writeln!(out, "ret")?;
    writeln!(out, "global _start")?;
    writeln!(out, "_start:")?;
    for op in program {
        match op {
            Op::Push(x) => {
                writeln!(out, "push {}", x)?;
            }
            Op::Plus => {
                writeln!(out, "pop rbx")?;
                writeln!(out, "pop rax")?;
                writeln!(out, "add rax, rbx")?;
                writeln!(out, "push rax")?;
            }
            Op::Minus => {
                writeln!(out, "pop rbx")?;
                writeln!(out, "pop rax")?;
                writeln!(out, "add rax, rbx")?;
                writeln!(out, "push rax")?;
            }
            Op::Dump => {
                writeln!(out, "pop rdi")?;
                writeln!(out, "call dump")?;
            }
        }
    }
    writeln!(out, "mov rax, 60")?;
    writeln!(out, "mov rdi, 0")?;
    writeln!(out, "syscall")?;
    Ok(())
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
        "com" => {
            if let Err(e) = compile_program(PROGRAM, "output.asm") {
                eprintln!("compiling program: {}", e);
                std::process::exit(1);
            }
        }
        _ => {
            usage(&args[0]);
            eprintln!("ERROR: unknown subcommand {}", subcommand);
            std::process::exit(1);
        }
    }
}
