const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

pub fn simulateProgram(comptime Writer: type, writer: Writer, allocator: Allocator, program: []const Op) !void {
    comptime if (!@hasDecl(Writer, "print")) {
        @compileError("invalid writer type");
    };
    var stack = std.ArrayList(u64).init(allocator);
    defer stack.deinit();
    var ip: usize = 0;
    while (ip < program.len) {
        const op = program[ip];
        switch (op.code) {
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
            .End => |target| {
                ip = target.?;
            },
            .Dup => {
                const value = stack.pop();
                try stack.append(value);
                try stack.append(value);
                ip += 1;
            },
            .Gt => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(if (a > b) 1 else 0);
                ip += 1;
            },
            .While => {
                ip += 1;
            },
            .Do => |target| {
                const value = stack.pop();
                if (value == 0) {
                    ip = target.?;
                } else {
                    ip += 1;
                }
            },
            .Dump => {
                const value = stack.pop();
                try writer.print("{d}\n", .{value});
                ip += 1;
            },
        }
    }
}
