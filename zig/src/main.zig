const std = @import("std");
const Op = @import("op.zig").Op;
const simulateProgram = @import("sim.zig").simulateProgram;
const compileProgram = @import("com.zig").compileProgram;
const argsParser = @import("args");

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
    const options = try argsParser.parseWithVerbForCurrentProcess(struct {}, union(enum) {
        sim: struct {},
        com: struct {
            run: bool = false,

            pub const shorthands = .{
                .r = "run",
            };
        },
    }, allocator.backing_allocator, .print);
    defer options.deinit();

    if (options.verb) |verb| {
        switch (verb) {
            .sim => {
                try simulateProgram(allocator.backing_allocator, &program);
            },
            .com => |comOptions| {
                try compileProgram(&program);
                if (comOptions.run) {
                    std.debug.print("Running...\n", .{});
                }
            },
        }
    } else {
        try usage(std.io.getStdErr().writer(), options.executable_name.?);
        return error.InvalidUsage;
    }
}

test "simulate program" {
    try simulateProgram(std.testing.allocator, &program);
}
