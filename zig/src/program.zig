const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

pub const Program = struct {
    ops: []Op,
    allocator: Allocator,

    const Self = @This();
    pub fn deinit(self: *Self) void {
        for (self.ops) |*op| {
            op.deinit(self.allocator);
        }
        self.allocator.free(self.ops);
    }
};
