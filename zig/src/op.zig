pub const Op = union(enum) {
    Push: u64,
    Plus,
    Minus,
    Dump,

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

    pub fn dump() Self {
        return Self.Dump;
    }
};
