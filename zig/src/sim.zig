const std = @import("std");
const Op = @import("op.zig").Op;
const memCapacity = @import("mem.zig").memCapacity;
const die = @import("die.zig").die;

const Allocator = std.mem.Allocator;

pub fn simulateProgram(comptime Writer: type, writer: Writer, allocator: Allocator, program: []const Op) !void {
    comptime if (!@hasDecl(Writer, "print")) {
        @compileError("invalid writer type");
    };
    var stack = std.ArrayList(u64).init(allocator);
    defer stack.deinit();
    var mem = try allocator.alloc(u8, memCapacity);
    defer allocator.free(mem);
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
            .Mem => {
                try stack.append(0);
                ip += 1;
            },
            .Load => {
                const addr = stack.pop();
                try stack.append(mem[addr]);
                ip += 1;
            },
            .Store => {
                const byte = stack.pop();
                const addr = stack.pop();
                mem[addr] = @truncate(u8, byte);
                ip += 1;
            },
            .Syscall1 => return error.NotImplemented,
            .Syscall2 => return error.NotImplemented,
            .Syscall3 => {
                const syscallNumber = stack.pop();
                const arg1 = stack.pop();
                const arg2 = stack.pop();
                const arg3 = stack.pop();
                switch (syscallNumber) {
                    1 => {
                        const fd = arg1;
                        const buf = arg2;
                        const count = arg3;
                        const s = mem[buf .. buf + count];
                        switch (fd) {
                            1 => {
                                try writer.print("{s}", .{s});
                            },
                            2 => {
                                std.debug.print("{s}", .{s});
                            },
                            else => die("Unknown file descriptor {d}\n", .{fd}),
                        }
                    },
                    else => die("Unknown syscall number: {d}\n", .{syscallNumber}),
                }
                ip += 1;
            },
            .Syscall4 => return error.NotImplemented,
            .Syscall5 => return error.NotImplemented,
            .Syscall6 => return error.NotImplemented,
            .Dup2 => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(a);
                try stack.append(b);
                try stack.append(a);
                try stack.append(b);
                ip += 1;
            },
            .Lt => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(if (a < b) 1 else 0);
                ip += 1;
            },
            .Swap => {
                const b = stack.pop();
                const a = stack.pop();
                try stack.append(b);
                try stack.append(a);
                ip += 1;
            },
        }
    }
}
