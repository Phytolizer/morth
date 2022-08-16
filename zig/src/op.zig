const std = @import("std");
const Token = @import("token.zig").Token;

const Allocator = std.mem.Allocator;

pub const Op = struct {
    token: Token,
    code: OpCode,

    pub const OpCode = union(enum) {
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

    const Self = @This();
    pub fn init(token: Token, code: OpCode) Self {
        return Self{ .token = token, .code = code };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.token.deinit(allocator);
    }
};
