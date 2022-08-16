const std = @import("std");
const Op = @import("op.zig").Op;
const toAbsolute = @import("path.zig").toAbsolute;

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

fn loadProgram(comptime Reader: type, allocator: Allocator, reader: Reader) ![]Op {
    var program = std.ArrayList(Op).init(allocator);
    errdefer program.deinit();

    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', maxLine)) |line| {
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

pub fn loadProgramFromText(allocator: Allocator, text: []const u8) ![]Op {
    var stream = std.io.fixedBufferStream(text);
    var streamReader = stream.reader();

    return try loadProgram(@TypeOf(streamReader), allocator, streamReader);
}

pub fn loadProgramFromFile(allocator: Allocator, filePath: []const u8) ![]Op {
    var program = std.ArrayList(Op).init(allocator);
    errdefer program.deinit();

    const inputFilePath = try toAbsolute(allocator, filePath);
    defer allocator.free(inputFilePath);

    var f = try std.fs.openFileAbsolute(inputFilePath, .{});
    defer f.close();
    const fReader = f.reader();

    return try loadProgram(@TypeOf(fReader), allocator, fReader);
}
