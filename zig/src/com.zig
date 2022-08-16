const std = @import("std");
const Op = @import("op.zig").Op;
const runCommand = @import("process.zig").runCommand;
const toAbsolute = @import("path.zig").toAbsolute;

const Allocator = std.mem.Allocator;

const prelude = @embedFile("prelude.nasm");
const epilogue = @embedFile("epilogue.nasm");
const dumpCSource = @embedFile("dump.c");

fn compileDumpSource(dumpOutputPath: []const u8, allocator: Allocator) !void {
    var dumpCPath = try toAbsolute(allocator, "dump.c");
    defer allocator.free(dumpCPath);
    {
        var dumpCFile = try std.fs.createFileAbsolute(dumpCPath, .{});
        defer dumpCFile.close();
        try dumpCFile.writeAll(dumpCSource);
    }
    var compileCommand = std.ChildProcess.init(&.{
        "zig",
        "cc",
        "-O2",
        "-o",
        dumpOutputPath,
        "-c",
        "-fno-stack-protector",
        dumpCPath,
    }, allocator);
    compileCommand.stdin_behavior = .Pipe;
    try compileCommand.spawn();
    try compileCommand.stdin.?.writeAll(dumpCSource);
    compileCommand.stdin.?.close();
    compileCommand.stdin = null;
    const ret = try compileCommand.wait();
    switch (ret) {
        .Exited => |code| {
            if (code == 0) {
                return;
            }
        },
        else => {},
    }

    return error.CommandFailed;
}

pub fn compileProgram(allocator: Allocator, program: []const Op) ![]u8 {
    const absPath = try toAbsolute(allocator, "output.nasm");
    defer allocator.free(absPath);
    const dumpOutputPath = try toAbsolute(allocator, "dump.o");
    defer allocator.free(dumpOutputPath);
    std.fs.accessAbsolute(dumpOutputPath, .{}) catch
        try compileDumpSource(dumpOutputPath, allocator);
    {
        var file = try std.fs.createFileAbsolute(absPath, .{});
        defer file.close();
        const fileWriter = file.writer();

        try fileWriter.writeAll(prelude);
        for (program) |op| {
            try fileWriter.print("    ;; -- {s} --\n", .{@tagName(op)});
            switch (op) {
                .op_push => |value| {
                    try fileWriter.print("    push {d}\n", .{value});
                },
                .op_plus => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    add rax, rdi\n");
                    try fileWriter.writeAll("    push rax\n");
                },
                .op_minus => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    sub rax, rdi\n");
                    try fileWriter.writeAll("    push rax\n");
                },
                .op_dump => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    call dump\n");
                },
            }
        }
        try fileWriter.writeAll(epilogue);
    }

    try runCommand(&.{
        "nasm",
        "-f",
        "elf64",
        "-o",
        "output.o",
        absPath,
    }, allocator);
    try runCommand(&.{
        "ld",
        "-o",
        "output",
        "-z",
        "noexecstack",
        "output.o",
        dumpOutputPath,
    }, allocator);

    return try toAbsolute(allocator, "output");
}
