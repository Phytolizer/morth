const std = @import("std");
const Op = @import("op.zig").Op;
const simulateProgram = @import("sim.zig").simulateProgram;
const compileProgram = @import("com.zig").compileProgram;

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
    const usageString =
        \\Usage: {s} <SUBCOMMAND> [ARGS]
        \\  SUBCOMMANDS:
        \\    sim               Simulate the program
        \\    com               Compile the program
        \\
    ;
    try writer.print(usageString, .{executableName});
}

pub fn main() !void {
    const allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const args = try std.process.argsAlloc(allocator.backing_allocator);
    defer std.process.argsFree(allocator.backing_allocator, args);

    if (args.len < 2) {
        try usage(std.io.getStdErr().writer(), args[0]);
        return error.InvalidUsage;
    }

    if (std.mem.eql(u8, args[1], "sim")) {
        try simulateProgram(allocator.backing_allocator, &program);
    } else if (std.mem.eql(u8, args[1], "com")) {
        try compileProgram(&program);
    } else {
        try usage(std.io.getStdErr().writer(), args[0]);
        return error.InvalidUsage;
    }
}

test "simulate program" {
    try simulateProgram(std.testing.allocator, &program);
}
