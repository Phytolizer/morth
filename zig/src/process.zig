const std = @import("std");

const Allocator = std.mem.Allocator;

pub const ShouldEcho = enum {
    Echo,
    NoEcho,
};

pub fn runCommand(shouldEcho: ShouldEcho, command: []const []const u8, allocator: Allocator) !void {
    switch (shouldEcho) {
        .Echo => {
            const stdout = std.io.getStdOut().writer();
            const commandJoined = try std.mem.join(allocator, " ", command);
            defer allocator.free(commandJoined);
            try stdout.print("[CMD] {s}\n", .{commandJoined});
        },
        .NoEcho => {},
    }
    var child = std.ChildProcess.init(command, allocator);
    try child.spawn();
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
