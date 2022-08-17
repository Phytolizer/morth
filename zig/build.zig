const std = @import("std");
const pkgs = @import("deps.zig").pkgs;

const Builder = std.build.Builder;
const LibExeObjStep = std.build.LibExeObjStep;

fn findLast(comptime T: type, slice: []const T, value: T) ?usize {
    var i: usize = slice.len;
    while (i > 0) : (i -= 1) {
        if (slice[i - 1] == value) {
            return i - 1;
        }
    }
    return null;
}

fn generateTests(b: *Builder) !*LibExeObjStep {
    const gpAllocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpAllocator.backing_allocator;

    const cwd = try std.process.getCwdAlloc(allocator);
    defer allocator.free(cwd);
    const baseDir = "src";
    const zigPath = try std.mem.concat(allocator, u8, &.{ baseDir, "/test.zig" });
    defer allocator.free(zigPath);
    const absoluteZigPath = try std.fs.path.join(allocator, &.{ try std.process.getCwdAlloc(allocator), zigPath });
    defer allocator.free(absoluteZigPath);
    var zigFile = try std.fs.createFileAbsolute(absoluteZigPath, .{});
    defer zigFile.close();
    const zigWriter = zigFile.writer();
    try zigWriter.writeAll(testHeader);

    const dir = try std.fs.cwd().openIterableDir("src/tests", .{});
    var dirIterator = dir.iterate();
    while (try dirIterator.next()) |path| {
        if (std.mem.endsWith(u8, path.name, ".morth")) {
            const absoluteMorthPath = try std.fs.path.join(allocator, &.{ cwd, baseDir, "tests", path.name });
            defer allocator.free(absoluteMorthPath);
            try generateTest(absoluteMorthPath, zigWriter);
        }
    }
    return b.addTest(absoluteZigPath);
}

const testHeader = @embedFile("test_header.zig_template");
const testTemplate = @embedFile("test.zig_template");

fn generateTest(morthPath: []const u8, zigWriter: std.fs.File.Writer) !void {
    const morthName = std.fs.path.basename(morthPath);
    const morthPeriod = findLast(u8, morthName, '.') orelse morthName.len;
    const morthBase = morthName[0..morthPeriod];
    try zigWriter.print(testTemplate, .{
        morthBase,
        morthPath,
        morthName,
        morthPath,
        morthBase,
    });
}

pub fn build(b: *Builder) !void {
    const target = b.standardTargetOptions(.{
        // Only supports x86_64-linux-gnu target.
        .whitelist = &.{
            .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu },
        },
    });

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

    const test_cmd = try generateTests(b);
    test_cmd.setTarget(target);
    test_cmd.setBuildMode(mode);
    test_step.dependOn(&test_cmd.step);
}
