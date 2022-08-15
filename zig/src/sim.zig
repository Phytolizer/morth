const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

pub fn simulateProgram(allocator: Allocator, program: []const Op) !void {
    const stdout = std.io.getStdOut().writer();
    var stack = std.ArrayList(u64).init(allocator);
    defer stack.deinit();
    for (program) |op| {
        switch (op) {
            .op_push => |value| {
                try stack.append(value);
            },
            .op_plus => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(a + b);
            },
            .op_minus => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(a - b);
            },
            .op_dump => {
                const value = stack.pop();
                try stdout.print("{d}\n", .{value});
            },
        }
    }
}
