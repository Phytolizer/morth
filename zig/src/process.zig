const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn runCommand(command: []const []const u8, allocator: Allocator) !void {
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
