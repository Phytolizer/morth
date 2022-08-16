const std = @import("std");
const Op = @import("op.zig").Op;

const Allocator = std.mem.Allocator;

const maxLine = 1024;

fn parseTokenAsOp(token: []const u8) !Op {
    if (std.mem.eql(u8, token, "+")) {
        return Op.plus();
    }
    if (std.mem.eql(u8, token, "-")) {
        return Op.minus();
    }
    if (std.mem.eql(u8, token, ".")) {
        return Op.dump();
    }

    const value = try std.fmt.parseInt(u64, token, 10);
    return Op.push(value);
}

pub fn loadProgramFromText(allocator: Allocator, text: []const u8) ![]Op {
    var program = std.ArrayList(Op).init(allocator);
    errdefer program.deinit();

    var stream = std.io.fixedBufferStream(text);
    var streamReader = stream.reader();

    while (try streamReader.readUntilDelimiterOrEofAlloc(allocator, '\n', maxLine)) |line| {
        defer allocator.free(line);

        var tokens = std.mem.tokenize(u8, line, " \t\r");
        while (tokens.next()) |tok| {
            const parsed = parseTokenAsOp(tok) catch
                return error.InvalidToken;
            try program.append(parsed);
        }
    }

    return program.toOwnedSlice();
}

pub fn loadProgramFromFile(allocator: Allocator, filePath: []const u8) ![]Op {
    var program = std.ArrayList(Op).init(allocator);
    errdefer program.deinit();

    const cwd = try std.process.getCwdAlloc(allocator);
    defer allocator.free(cwd);
    var inputFilePath = if (!std.fs.path.isAbsolute(filePath))
        try std.fs.path.join(allocator, &.{ cwd, filePath })
    else
        try allocator.dupe(u8, filePath);
    defer allocator.free(inputFilePath);

    {
        var f = try std.fs.openFileAbsolute(inputFilePath, .{});
        defer f.close();
        const fReader = f.reader();

        while (try fReader.readUntilDelimiterOrEofAlloc(allocator, '\n', maxLine)) |line| {
            defer allocator.free(line);

            var tokens = std.mem.tokenize(u8, line, " \t\r");
            while (tokens.next()) |tok| {
                const parsed = parseTokenAsOp(tok) catch
                    return error.InvalidToken;
                try program.append(parsed);
            }
        }
    }

    return program.toOwnedSlice();
}
