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
    try simulateProgram(std.testing.allocator, program);
    const exePath = try compileProgram(std.testing.allocator, program, path);
    defer std.testing.allocator.free(exePath);
    try runCommand(.NoEcho, &.{exePath}, std.testing.allocator);
}
