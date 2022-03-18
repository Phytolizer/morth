use crate::{op::Op, token::Token, Error, Result, Value};

pub(crate) fn parse_token_as_op(tok: Token) -> Result<Op> {
    match tok.text.as_str() {
        "+" => Ok(Op::plus(tok.location)),
        "-" => Ok(Op::minus(tok.location)),
        "." => Ok(Op::dump(tok.location)),
        _ => {
            let value = tok
                .text
                .parse::<Value>()
                .map_err(|_| Error::UnknownToken(tok.text))?;
            Ok(Op::push(value, tok.location))
        }
    }
}
