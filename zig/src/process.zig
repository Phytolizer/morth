const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn RunCommandOptions(comptime Writer: type) type {
    return struct {
        shouldEcho: bool = false,
        writer: Writer,
    };
}

pub fn runCommand(comptime Writer: type, options: RunCommandOptions(Writer), command: []const []const u8, allocator: Allocator) !void {
    comptime if (!(@hasDecl(Writer, "writeAll") and @hasDecl(Writer, "print"))) {
        @compileError("Writer does not have a writeAll and print method");
    };
    if (options.shouldEcho) {
        const commandJoined = try std.mem.join(allocator, " ", command);
        defer allocator.free(commandJoined);
        try options.writer.print("[CMD] {s}\n", .{commandJoined});
    }
    var child = std.ChildProcess.init(command, allocator);
    child.stdout_behavior = .Pipe;
    try child.spawn();
    var buffer: [64]u8 = undefined;
    while (true) {
        const bytesRead = try child.stdout.?.read(&buffer);
        if (bytesRead == 0) {
            break;
        }
        try options.writer.writeAll(buffer[0..bytesRead]);
    }
    child.stdout.?.close();
    child.stdout = null;
    const ret = try child.wait();
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
