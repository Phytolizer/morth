use location::SourceLocation;

type Value = u64;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unknown token '{0}'")]
    UnknownToken(String),
    #[error("i/o error: {0}")]
    Io(#[from] std::io::Error),
    #[error("stack underflow at {0}")]
    StackUnderflow(SourceLocation),
}

pub type Result<T> = std::result::Result<T, Error>;

mod lex;
mod location;
mod op;
mod parse;
pub mod sim;
mod token;
