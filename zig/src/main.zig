const std = @import("std");
const Op = @import("op.zig").Op;
const simulateProgram = @import("sim.zig").simulateProgram;
const compileProgram = @import("com.zig").compileProgram;
const argsParser = @import("args");
const runCommand = @import("process.zig").runCommand;
const loadProgramFromFile = @import("load.zig").loadProgramFromFile;
const loadProgramFromText = @import("load.zig").loadProgramFromText;
const toAbsolute = @import("path.zig").toAbsolute;
const crossReferenceBlocks = @import("crossReference.zig").crossReferenceBlocks;

const Writer = std.fs.File.Writer;

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
            output: ?[]const u8 = null,

            pub const shorthands = .{
                .r = "run",
                .o = "output",
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
                if (parsed.positionals.len == 0) {
                    try usage(std.io.getStdErr().writer(), parsed.executable_name.?);
                    return error.InvalidUsage;
                }
                const inputFilePath = parsed.positionals[0];
                var program = try loadProgramFromFile(allocator.backing_allocator, inputFilePath);
                defer program.deinit();
                try crossReferenceBlocks(allocator.backing_allocator, program.ops);
                const stdout = std.io.getStdOut().writer();
                try simulateProgram(@TypeOf(stdout), stdout, allocator.backing_allocator, program.ops);
            },
            .com => |comOptions| {
                if (parsed.positionals.len == 0) {
                    try usage(std.io.getStdErr().writer(), parsed.executable_name.?);
                    return error.InvalidUsage;
                }
                const inputFilePath = try toAbsolute(allocator.backing_allocator, parsed.positionals[0]);
                defer allocator.backing_allocator.free(inputFilePath);
                var program = try loadProgramFromFile(allocator.backing_allocator, inputFilePath);
                defer program.deinit();
                try crossReferenceBlocks(allocator.backing_allocator, program.ops);
                const outputPath = comOptions.output;
                const exePath = try compileProgram(allocator.backing_allocator, program.ops, inputFilePath, outputPath);
                defer allocator.backing_allocator.free(exePath);
                if (comOptions.run) {
                    const stdout = std.io.getStdOut().writer();
                    try runCommand(
                        @TypeOf(stdout),
                        .{ .shouldEcho = true, .writer = stdout },
                        &.{exePath},
                        allocator.backing_allocator,
                    );
                }
            },
        }
    } else {
        try usage(std.io.getStdErr().writer(), parsed.executable_name.?);
        return error.InvalidUsage;
    }
}
