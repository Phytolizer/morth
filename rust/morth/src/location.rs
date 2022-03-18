use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub(crate) file_name: String,
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file_name, self.line, self.column)
    }
}
