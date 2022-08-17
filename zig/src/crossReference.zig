const std = @import("std");
const Op = @import("op.zig").Op;
const Token = @import("token.zig").Token;
const die = @import("die.zig").die;

const Allocator = std.mem.Allocator;

fn reportError(token: Token, message: []const u8) noreturn {
    die(
        "{s}:{d}:{d}: {s}\n",
        .{ token.filePath, token.row, token.col, message },
    );
}

pub fn crossReferenceBlocks(allocator: Allocator, program: []Op) !void {
    var stack = std.ArrayList(usize).init(allocator);
    defer stack.deinit();
    var ip: usize = 0;
    while (ip < program.len) : (ip += 1) {
        const op = &program[ip];
        switch (op.code) {
            .If => {
                try stack.append(ip);
            },
            .Else => {
                const errorMessage = "ERROR: `else` can only be used in `if` blocks";
                const ifIp = stack.popOrNull() orelse
                    reportError(op.token, errorMessage);
                switch (program[ifIp].code) {
                    .If => |*target| {
                        target.* = ip + 1;
                    },
                    else => reportError(op.token, errorMessage),
                }
                try stack.append(ip);
            },
            .End => |*endTarget| {
                const errorMessage = "ERROR: `end` can only be used to close `if`, `else`, or `do` blocks";
                const blockIp = stack.popOrNull() orelse
                    reportError(op.token, errorMessage);
                switch (program[blockIp].code) {
                    .If, .Else => |*target| {
                        target.* = ip;
                        endTarget.* = ip + 1;
                    },
                    .Do => |*target| {
                        endTarget.* = target.*;
                        target.* = ip + 1;
                    },
                    else => reportError(op.token, errorMessage),
                }
            },
            .While => {
                try stack.append(ip);
            },
            .Do => |*target| {
                const errorMessage = "ERROR: `do` can only be used in `while` blocks";
                const whileIp = stack.popOrNull() orelse
                    reportError(op.token, errorMessage);
                switch (program[whileIp].code) {
                    .While => {
                        target.* = whileIp;
                    },
                    else => reportError(op.token, errorMessage),
                }
                try stack.append(ip);
            },
            else => {},
        }
    }
    if (stack.items.len > 0) {
        const top = program[stack.items[stack.items.len - 1]];
        reportError(top.token, "ERROR: unterminated block");
    }
}
