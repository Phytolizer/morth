const std = @import("std");
const Op = @import("op.zig").Op;
const runCommand = @import("process.zig").runCommand;
const toAbsolute = @import("path.zig").toAbsolute;
const memCapacity = @import("mem.zig").memCapacity;

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

fn computeBaseName(path: []const u8) []const u8 {
    const morthExt = ".morth";
    if (std.mem.endsWith(u8, path, morthExt)) {
        return path[0 .. path.len - morthExt.len];
    }
    return path;
}

pub fn compileProgram(allocator: Allocator, program: []const Op, sourcePath: []const u8, outputPathArg: ?[]const u8) ![]u8 {
    const basename = computeBaseName(sourcePath);
    const outputPath = try std.mem.concat(allocator, u8, &.{ basename, ".nasm" });
    defer allocator.free(outputPath);
    const dumpOutputPath = try toAbsolute(allocator, "dump.o");
    defer allocator.free(dumpOutputPath);
    std.fs.accessAbsolute(dumpOutputPath, .{}) catch
        try compileDumpSource(dumpOutputPath, allocator);
    {
        var file = try std.fs.createFileAbsolute(outputPath, .{});
        defer file.close();
        const fileWriter = file.writer();

        try fileWriter.writeAll(prelude);
        var ip: usize = 0;
        while (ip < program.len) : (ip += 1) {
            const op = program[ip];
            try fileWriter.print(".morth_addr_{d}:\n", .{ip});
            try fileWriter.print("    ;; -- {s} --\n", .{@tagName(op.code)});
            switch (op.code) {
                .Push => |value| {
                    try fileWriter.print("    push {d}\n", .{value});
                },
                .Plus => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    add rax, rdi\n");
                    try fileWriter.writeAll("    push rax\n");
                },
                .Minus => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    sub rax, rdi\n");
                    try fileWriter.writeAll("    push rax\n");
                },
                .Equal => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    cmp rax, rdi\n");
                    try fileWriter.writeAll("    sete al\n");
                    try fileWriter.writeAll("    movzx rax, al\n");
                    try fileWriter.writeAll("    push rax\n");
                },
                .If => |target| {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    cmp rax, 0\n");
                    try fileWriter.print("    je .morth_addr_{d}\n", .{target.?});
                },
                .Else => |target| {
                    try fileWriter.print("    jmp .morth_addr_{d}\n", .{target.?});
                },
                .End => |target| {
                    try fileWriter.print("    jmp .morth_addr_{d}\n", .{target.?});
                },
                .Dup => {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    push rax\n");
                    try fileWriter.writeAll("    push rax\n");
                },
                .Gt => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    cmp rax, rdi\n");
                    try fileWriter.writeAll("    setg al\n");
                    try fileWriter.writeAll("    movzx rax, al\n");
                    try fileWriter.writeAll("    push rax\n");
                },
                .Dump => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    call dump\n");
                },
                .While => {},
                .Do => |target| {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    cmp rax, 0\n");
                    try fileWriter.print("    je .morth_addr_{d}\n", .{target.?});
                },
                .Mem => {
                    try fileWriter.writeAll("    push mem\n");
                },
                .Load => {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    mov rbx, 0\n");
                    try fileWriter.writeAll("    mov bl, [rax]\n");
                    try fileWriter.writeAll("    push rbx\n");
                },
                .Store => {
                    try fileWriter.writeAll("    pop rbx\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    mov [rax], bl\n");
                },
                .Syscall1 => {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    syscall\n");
                },
                .Syscall2 => {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rsi\n");
                    try fileWriter.writeAll("    syscall\n");
                },
                .Syscall3 => {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rsi\n");
                    try fileWriter.writeAll("    pop rdx\n");
                    try fileWriter.writeAll("    syscall\n");
                },
                .Syscall4 => {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rsi\n");
                    try fileWriter.writeAll("    pop rdx\n");
                    try fileWriter.writeAll("    pop r10\n");
                    try fileWriter.writeAll("    syscall\n");
                },
                .Syscall5 => {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rsi\n");
                    try fileWriter.writeAll("    pop rdx\n");
                    try fileWriter.writeAll("    pop r10\n");
                    try fileWriter.writeAll("    pop r8\n");
                    try fileWriter.writeAll("    syscall\n");
                },
                .Syscall6 => {
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rsi\n");
                    try fileWriter.writeAll("    pop rdx\n");
                    try fileWriter.writeAll("    pop r10\n");
                    try fileWriter.writeAll("    pop r8\n");
                    try fileWriter.writeAll("    pop r9\n");
                    try fileWriter.writeAll("    syscall\n");
                },
                .Dup2 => {
                    try fileWriter.writeAll("    pop rbx\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    push rax\n");
                    try fileWriter.writeAll("    push rbx\n");
                    try fileWriter.writeAll("    push rax\n");
                    try fileWriter.writeAll("    push rbx\n");
                },
                .Lt => {
                    try fileWriter.writeAll("    pop rdi\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    cmp rax, rdi\n");
                    try fileWriter.writeAll("    setl al\n");
                    try fileWriter.writeAll("    movzx rax, al\n");
                    try fileWriter.writeAll("    push rax\n");
                },
                .Swap => {
                    try fileWriter.writeAll("    pop rbx\n");
                    try fileWriter.writeAll("    pop rax\n");
                    try fileWriter.writeAll("    push rbx\n");
                    try fileWriter.writeAll("    push rax\n");
                },
            }
        }
        try fileWriter.print(".morth_addr_{d}:\n", .{program.len});
        try fileWriter.print(epilogue, .{memCapacity});
    }

    const objPath = try std.mem.concat(allocator, u8, &.{ basename, ".o" });
    defer allocator.free(objPath);
    const stdout = std.io.getStdOut().writer();
    try runCommand(
        @TypeOf(stdout),
        .{ .shouldEcho = true, .writer = stdout },
        &.{
            "nasm",
            "-f",
            "elf64",
            "-o",
            objPath,
            outputPath,
        },
        allocator,
    );
    const exePath = try toAbsolute(allocator, outputPathArg orelse basename);
    try runCommand(
        @TypeOf(stdout),
        .{ .shouldEcho = true, .writer = stdout },
        &.{
            "ld",
            "-o",
            exePath,
            "-z",
            "noexecstack",
            objPath,
            dumpOutputPath,
        },
        allocator,
    );

    return exePath;
}
