const std = @import("std");
const Op = @import("op.zig").Op;
const simulateProgram = @import("sim.zig").simulateProgram;
const compileProgram = @import("com.zig").compileProgram;
const argsParser = @import("args");
const runCommand = @import("process.zig").runCommand;

const Writer = std.fs.File.Writer;

const program = [_]Op{
    Op.push(34),
    Op.push(35),
    Op.plus(),
    Op.dump(),
    Op.push(500),
    Op.push(80),
    Op.minus(),
    Op.dump(),
};

fn usage(writer: Writer, executableName: []const u8) !void {
    const usageString = @embedFile("usage.txt");
    try writer.print(usageString, .{executableName});
}

pub fn main() !void {
    const allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const parsed = try argsParser.parseWithVerbForCurrentProcess(struct {
        help: bool = false,

        pub const shorthands = .{
            .h = "help",
        };
    }, union(enum) {
        sim: struct {},
        com: struct {
            run: bool = false,

            pub const shorthands = .{
                .r = "run",
            };
        },
    }, allocator.backing_allocator, .print);
    defer parsed.deinit();

    if (parsed.options.help) {
        try usage(std.io.getStdOut().writer(), parsed.executable_name.?);
        return;
    }

    if (parsed.verb) |verb| {
        switch (verb) {
            .sim => {
                try simulateProgram(allocator.backing_allocator, &program);
            },
            .com => |comOptions| {
                const exePath = try compileProgram(allocator.backing_allocator, &program);
                defer allocator.backing_allocator.free(exePath);
                if (comOptions.run) {
                    try runCommand(&.{exePath}, allocator.backing_allocator);
                }
            },
        }
    } else {
        try usage(std.io.getStdErr().writer(), parsed.executable_name.?);
        return error.InvalidUsage;
    }
}

test "simulate program" {
    try simulateProgram(std.testing.allocator, &program);
}
