const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

pub fn crossReferenceBlocks(allocator: Allocator, program: []Op) !void {
    var stack = std.ArrayList(usize).init(allocator);
    defer stack.deinit();
    var ip: usize = 0;
    while (ip < program.len) : (ip += 1) {
        const op = &program[ip];
        switch (op.*) {
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
                        return error.MismatchedElse;
                    },
                }
                try stack.append(ip);
            },
            .End => |*endTarget| {
                const blockIp = stack.pop();
                switch (program[blockIp]) {
                    .If, .Else => |*target| {
                        target.* = ip;
                        endTarget.* = ip + 1;
                    },
                    .Do => |*target| {
                        endTarget.* = target.*;
                        target.* = ip + 1;
                    },
                    else => {
                        return error.MismatchedEnd;
                    },
                }
            },
            .While => {
                try stack.append(ip);
            },
            .Do => |*target| {
                const whileIp = stack.pop();
                switch (program[whileIp]) {
                    .While => {
                        target.* = whileIp;
                    },
                    else => {
                        return error.MismatchedDo;
                    },
                }
                try stack.append(ip);
            },
            else => {},
        }
    }
}
