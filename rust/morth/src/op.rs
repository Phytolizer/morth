use crate::{location::SourceLocation, Value};

pub(crate) enum OpCode {
    Push(Value),
    Plus,
    Minus,
    Dump,
}

pub(crate) struct Op {
    pub(crate) code: OpCode,
    pub(crate) location: SourceLocation,
}

impl Op {
    pub(crate) fn push(value: Value, location: SourceLocation) -> Self {
        Self {
            code: OpCode::Push(value),
            location,
        }
    }

    pub(crate) fn plus(location: SourceLocation) -> Self {
        Self {
            code: OpCode::Plus,
            location,
        }
    }

    pub(crate) fn minus(location: SourceLocation) -> Self {
        Self {
            code: OpCode::Minus,
            location,
        }
    }

    pub(crate) fn dump(location: SourceLocation) -> Self {
        Self {
            code: OpCode::Dump,
            location,
        }
    }
}
