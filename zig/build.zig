const std = @import("std");
const pkgs = @import("deps.zig").pkgs;

fn findLast(comptime T: type, slice: []const T, value: T) ?usize {
    var i: usize = slice.len;
    while (i > 0) : (i -= 1) {
        if (slice[i - 1] == value) {
            return i - 1;
        }
    }
    return null;
}

const testTemplate = @embedFile("test.zig_template");

fn generateTest(b: *std.build.Builder, target: std.zig.CrossTarget, mode: std.builtin.Mode, testStep: *std.build.Step, morthPathIn: []const u8) !void {
    const gpAllocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpAllocator.backing_allocator;
    const morthPath = try std.fs.path.join(allocator, &.{ "src", morthPathIn });
    defer allocator.free(morthPath);
    const lastSlash = findLast(u8, morthPath, '/') orelse 0;
    const baseDir = if (morthPath[0..lastSlash].len > 0)
        morthPath[0..lastSlash]
    else
        ".";
    const cwd = try std.process.getCwdAlloc(allocator);
    defer allocator.free(cwd);
    const absoluteMorthPath = try std.fs.path.join(allocator, &.{ cwd, "src", "tests", morthPathIn });
    defer allocator.free(absoluteMorthPath);
    const morthName = std.fs.path.basename(morthPath);
    const morthPeriod = findLast(u8, morthName, '.') orelse morthName.len;
    const morthBase = morthName[0..morthPeriod];
    const zigPath = try std.mem.concat(allocator, u8, &.{ baseDir, "/test_", morthBase, ".zig" });
    defer allocator.free(zigPath);
    const absoluteZigPath = try std.fs.path.join(allocator, &.{ try std.process.getCwdAlloc(allocator), zigPath });
    defer allocator.free(absoluteZigPath);
    {
        var file = try std.fs.createFileAbsolute(absoluteZigPath, .{});
        defer file.close();
        try file.writer().print(testTemplate, .{ morthPathIn, morthName, absoluteMorthPath });
    }
    const testCmd = b.addTest(zigPath);
    testCmd.setTarget(target);
    testCmd.setBuildMode(mode);
    testStep.dependOn(&testCmd.step);
}

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("morth", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();
    pkgs.addAllTo(exe);

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run unit tests");

    const dir = std.fs.cwd().openIterableDir("src/tests", .{}) catch unreachable;
    var dirIterator = dir.iterate();

    while (dirIterator.next() catch unreachable) |path| {
        if (std.mem.endsWith(u8, path.name, ".morth")) {
            generateTest(b, target, mode, test_step, path.name) catch unreachable;
        }
    }
}
