const std = @import("std");
const simulateProgram = @import("sim.zig").simulateProgram;
const compileProgram = @import("com.zig").compileProgram;
const loadProgramFromText = @import("load.zig").loadProgramFromText;
const crossReferenceBlocks = @import("crossReference.zig").crossReferenceBlocks;
const runCommand = @import("process.zig").runCommand;

pub fn doTest(comptime path: []const u8, comptime source: []const u8) !void {
    const program = try loadProgramFromText(std.testing.allocator, source);
    defer std.testing.allocator.free(program);
    try crossReferenceBlocks(std.testing.allocator, program);
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    var bufWriter = buffer.writer();
    try simulateProgram(@TypeOf(bufWriter), bufWriter, std.testing.allocator, program);
    const simOutput = buffer.toOwnedSlice();
    defer std.testing.allocator.free(simOutput);
    const exePath = try compileProgram(std.testing.allocator, program, path, null);
    defer std.testing.allocator.free(exePath);
    try runCommand(
        @TypeOf(bufWriter),
        .{ .shouldEcho = .NoEcho, .writer = bufWriter },
        &.{exePath},
        std.testing.allocator,
    );
    const comOutput = buffer.toOwnedSlice();
    defer std.testing.allocator.free(comOutput);
    try std.testing.expectEqualStrings(simOutput, comOutput);
}
