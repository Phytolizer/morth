const std = @import("std");
const Op = @import("op.zig").Op;
const toAbsolute = @import("path.zig").toAbsolute;

const Allocator = std.mem.Allocator;

const maxLine = 1024;

const Token = struct {
    filePath: []const u8,
    row: u64,
    col: u64,
    word: []u8,

    const Self = @This();

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.word);
    }
};

fn parseTokenAsOp(token: Token) !Op {
    if (std.mem.eql(u8, token.word, "+")) {
        return Op.Plus;
    }
    if (std.mem.eql(u8, token.word, "-")) {
        return Op.Minus;
    }
    if (std.mem.eql(u8, token.word, ".")) {
        return Op.Dump;
    }
    if (std.mem.eql(u8, token.word, "=")) {
        return Op.Equal;
    }
    if (std.mem.eql(u8, token.word, "if")) {
        return Op{ .If = null };
    }
    if (std.mem.eql(u8, token.word, "else")) {
        return Op{ .Else = null };
    }
    if (std.mem.eql(u8, token.word, "end")) {
        return Op{ .End = null };
    }
    if (std.mem.eql(u8, token.word, "dup")) {
        return Op.Dup;
    }
    if (std.mem.eql(u8, token.word, ">")) {
        return Op.Gt;
    }
    if (std.mem.eql(u8, token.word, "while")) {
        return Op.While;
    }
    if (std.mem.eql(u8, token.word, "do")) {
        return Op{ .Do = null };
    }

    const value = std.fmt.parseInt(u64, token.word, 10) catch {
        std.debug.print("{s}:{d}:{d}: Invalid number '{s}'\n", .{ token.filePath, token.row, token.col, token.word });
        std.process.exit(1);
    };
    return Op{ .Push = value };
}

fn findCol(line: []const u8, start: usize, predicate: fn (u8) bool) usize {
    var col = start;
    while (col < line.len and !predicate(line[col])) {
        col += 1;
    }
    return col;
}

fn isNotSpace(c: u8) bool {
    return !std.ascii.isSpace(c);
}

fn loadProgram(comptime Reader: type, filePath: []const u8, allocator: Allocator, reader: Reader) ![]Op {
    var program = std.ArrayList(Op).init(allocator);
    errdefer program.deinit();

    var lineNumber: u64 = 1;

    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', maxLine)) |line| : (lineNumber += 1) {
        defer allocator.free(line);

        var col = findCol(line, 0, isNotSpace);
        while (col < line.len) {
            const colEnd = findCol(line, col, std.ascii.isSpace);
            const token = Token{
                .filePath = filePath,
                .row = lineNumber,
                .col = col + 1,
                .word = line[col..colEnd],
            };
            try program.append(try parseTokenAsOp(token));
            col = findCol(line, colEnd, isNotSpace);
        }
    }

    return program.toOwnedSlice();
}

pub fn loadProgramFromText(allocator: Allocator, text: []const u8) ![]Op {
    var stream = std.io.fixedBufferStream(text);
    var streamReader = stream.reader();

    return try loadProgram(@TypeOf(streamReader), "<memory>", allocator, streamReader);
}

pub fn loadProgramFromFile(allocator: Allocator, filePath: []const u8) ![]Op {
    var program = std.ArrayList(Op).init(allocator);
    errdefer program.deinit();

    const inputFilePath = try toAbsolute(allocator, filePath);
    defer allocator.free(inputFilePath);

    var f = try std.fs.openFileAbsolute(inputFilePath, .{});
    defer f.close();
    const fReader = f.reader();

    return try loadProgram(@TypeOf(fReader), inputFilePath, allocator, fReader);
}
