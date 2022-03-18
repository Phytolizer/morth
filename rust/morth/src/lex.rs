use std::io::{BufRead, BufReader, Read};

use crate::{location::SourceLocation, token::Token, Result};

fn lex_line(
    file_name: &str,
    line_index: usize,
    line: std::io::Result<String>,
) -> Result<Vec<Token>> {
    let line = line?.chars().collect::<Vec<_>>();
    let mut tokens = Vec::new();
    let mut prev_token_start = 0;
    loop {
        let token_start = match line
            .iter()
            .skip(prev_token_start)
            .position(|c| !c.is_whitespace())
        {
            Some(s) => s,
            None => return Ok(tokens),
        };
        let token_length = line
            .iter()
            .skip(token_start)
            .position(|c| c.is_whitespace())
            .unwrap_or_else(|| line.len() - token_start);
        tokens.push(Token {
            location: SourceLocation {
                file_name: file_name.to_string(),
                line: line_index,
                column: token_start,
            },
            text: line
                [prev_token_start + token_start..prev_token_start + token_start + token_length]
                .iter()
                .collect::<String>(),
        });
        prev_token_start += token_start;
    }
}

pub(crate) fn lex_program(file_name: &str, input: &mut dyn Read) -> Result<Vec<Token>> {
    Ok(BufReader::new(input)
        .lines()
        .enumerate()
        .map(|(line_index, line)| lex_line(file_name, line_index, line))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect())
}
