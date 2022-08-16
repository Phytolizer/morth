const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

pub fn simulateProgram(allocator: Allocator, program: []const Op) !void {
    const stdout = std.io.getStdOut().writer();
    var stack = std.ArrayList(u64).init(allocator);
    defer stack.deinit();
    for (program) |op| {
        switch (op) {
            .Push => |value| {
                try stack.append(value);
            },
            .Plus => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(a + b);
            },
            .Minus => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(a - b);
            },
            .Dump => {
                const value = stack.pop();
                try stdout.print("{d}\n", .{value});
            },
        }
    }
}
