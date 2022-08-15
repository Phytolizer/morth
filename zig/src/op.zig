pub const OpCode = enum {
    op_push,
    op_plus,
    op_minus,
    op_dump,
};

pub const Op = union(OpCode) {
    op_push: u64,
    op_plus,
    op_minus,
    op_dump,

    const Self = @This();

    pub fn push(value: u64) Self {
        return Self{ .op_push = value };
    }

    pub fn plus() Self {
        return Self.op_plus;
    }

    pub fn minus() Self {
        return Self.op_minus;
    }

    pub fn dump() Self {
        return Self.op_dump;
    }
};
