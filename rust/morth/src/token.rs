use crate::location::SourceLocation;

pub(crate) struct Token {
    pub(crate) text: String,
    pub(crate) location: SourceLocation,
}
