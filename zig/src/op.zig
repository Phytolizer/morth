pub const Op = union(enum) {
    Push: u64,
    Plus,
    Minus,
    Equal,
    Dump,
    If: ?usize,
    Else: ?usize,
    End,

    const Self = @This();

    pub fn push(value: u64) Self {
        return Self{ .Push = value };
    }

    pub fn plus() Self {
        return Self.Plus;
    }

    pub fn minus() Self {
        return Self.Minus;
    }

    pub fn equal() Self {
        return Self.Equal;
    }

    pub fn dump() Self {
        return Self.Dump;
    }

    pub fn iff() Self {
        return Self{ .If = null };
    }

    pub fn elze() Self {
        return Self{ .Else = null };
    }

    pub fn end() Self {
        return Self.End;
    }
};
