const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

pub fn simulateProgram(allocator: Allocator, program: []const Op) !void {
    const stdout = std.io.getStdOut().writer();
    var stack = std.ArrayList(u64).init(allocator);
    defer stack.deinit();
    var ip: usize = 0;
    while (ip < program.len) {
        const op = program[ip];
        switch (op) {
            .Push => |value| {
                try stack.append(value);
                ip += 1;
            },
            .Plus => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(a + b);
                ip += 1;
            },
            .Minus => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(a - b);
                ip += 1;
            },
            .Equal => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(if (a == b) 1 else 0);
                ip += 1;
            },
            .If => |target| {
                const value = stack.pop();
                if (value == 0) {
                    ip = target.?;
                } else {
                    ip += 1;
                }
            },
            .Else => |target| {
                ip = target.?;
            },
            .End => {
                ip += 1;
            },
            .Dup => {
                const value = stack.pop();
                try stack.append(value);
                try stack.append(value);
                ip += 1;
            },
            .Dump => {
                const value = stack.pop();
                try stdout.print("{d}\n", .{value});
                ip += 1;
            },
        }
    }
}
