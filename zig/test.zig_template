const std = @import("std");
const simulateProgram = @import("sim.zig").simulateProgram;
const compileProgram = @import("com.zig").compileProgram;
const loadProgramFromText = @import("load.zig").loadProgramFromText;
const crossReferenceBlocks = @import("crossReference.zig").crossReferenceBlocks;
const runCommand = @import("process.zig").runCommand;

const source = @embedFile("tests/{s}");

test "{s}" {{
    const program = try loadProgramFromText(std.testing.allocator, source);
    defer std.testing.allocator.free(program);
    try crossReferenceBlocks(std.testing.allocator, program);
    try simulateProgram(std.testing.allocator, program);
    const exePath = try compileProgram(std.testing.allocator, program, "{s}");
    defer std.testing.allocator.free(exePath);
    try runCommand(.NoEcho, &.{{exePath}}, std.testing.allocator);
}}
