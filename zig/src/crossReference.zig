const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

pub fn crossReferenceBlocks(allocator: Allocator, program: []Op) !void {
    var stack = std.ArrayList(usize).init(allocator);
    var ip: usize = 0;
    while (ip < program.len) : (ip += 1) {
        const op = program[ip];
        switch (op) {
            .If => {
                try stack.append(ip);
            },
            .End => {
                const ifIp = stack.pop();
                switch (program[ifIp]) {
                    .If => |*target| {
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
