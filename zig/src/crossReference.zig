const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

pub fn crossReferenceBlocks(allocator: Allocator, program: []Op) !void {
    var stack = std.ArrayList(usize).init(allocator);
    defer stack.deinit();
    var ip: usize = 0;
    while (ip < program.len) : (ip += 1) {
        const op = program[ip];
        switch (op) {
            .If => {
                try stack.append(ip);
            },
            .Else => {
                const ifIp = stack.pop();
                switch (program[ifIp]) {
                    .If => |*target| {
                        target.* = ip + 1;
                    },
                    else => {
                        return error.MismathedElse;
                    },
                }
                try stack.append(ip);
            },
            .End => {
                const blockIp = stack.pop();
                switch (program[blockIp]) {
                    .If => |*target| {
                        target.* = ip;
                    },
                    .Else => |*target| {
                        target.* = ip;
                    },
                    else => {
                        return error.MismatchedEnd;
                    },
                }
            },
            else => {},
        }
    }
}
