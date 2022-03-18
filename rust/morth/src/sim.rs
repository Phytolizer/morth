use std::io::{Read, Write};

use crate::{
    lex::lex_program,
    op::{Op, OpCode},
    parse::parse_token_as_op,
    Error, Result, Value,
};

pub fn simulate_program(
    source_filename: &str,
    source: &mut dyn Read,
    output: &mut dyn Write,
) -> Result<()> {
    let program = lex_program(source_filename, source)?
        .into_iter()
        .map(parse_token_as_op)
        .collect::<Result<Vec<_>>>()?;
    simulate(&program, output)?;
    Ok(())
}

fn simulate(program: &[Op], output: &mut dyn Write) -> Result<()> {
    let mut stack = Vec::<Value>::new();
    let mut ip = 0;
    while ip < program.len() {
        match program[ip].code {
            OpCode::Push(value) => {
                stack.push(value);
                ip += 1;
            }
            OpCode::Plus => {
                let b = stack
                    .pop()
                    .ok_or_else(|| Error::StackUnderflow(program[ip].location.clone()))?;
                let a = stack
                    .pop()
                    .ok_or_else(|| Error::StackUnderflow(program[ip].location.clone()))?;
                stack.push(a + b);
                ip += 1;
            }
            OpCode::Minus => {
                let b = stack
                    .pop()
                    .ok_or_else(|| Error::StackUnderflow(program[ip].location.clone()))?;
                let a = stack
                    .pop()
                    .ok_or_else(|| Error::StackUnderflow(program[ip].location.clone()))?;
                stack.push(a - b);
                ip += 1;
            }
            OpCode::Dump => {
                let value = stack
                    .pop()
                    .ok_or_else(|| Error::StackUnderflow(program[ip].location.clone()))?;
                writeln!(output, "{}", value)?;
                ip += 1;
            }
        }
    }
    Ok(())
}
