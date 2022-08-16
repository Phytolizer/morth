pub const Op = union(enum) {
    Push: u64,
    Plus,
    Minus,
    Equal,
    Dump,
    If: ?usize,
    Else: ?usize,
    End: ?usize,
    Dup,
    Gt,
    While,
    Do: ?usize,
};
