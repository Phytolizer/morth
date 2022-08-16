const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Token = struct {
    filePath: []u8,
    row: u64,
    col: u64,
    word: []u8,

    const Self = @This();

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.word);
    }
};
