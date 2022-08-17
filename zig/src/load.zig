const std = @import("std");
const Op = @import("op.zig").Op;
const toAbsolute = @import("path.zig").toAbsolute;
const Token = @import("token.zig").Token;
const Program = @import("program.zig").Program;

const Allocator = std.mem.Allocator;

const maxLine = 1024;

fn parseTokenAsOp(token: Token) !Op {
    if (std.mem.eql(u8, token.word, "+")) {
        return Op.init(token, .Plus);
    }
    if (std.mem.eql(u8, token.word, "-")) {
        return Op.init(token, .Minus);
    }
    if (std.mem.eql(u8, token.word, "dump")) {
        return Op.init(token, .Dump);
    }
    if (std.mem.eql(u8, token.word, "=")) {
        return Op.init(token, .Equal);
    }
    if (std.mem.eql(u8, token.word, "if")) {
        return Op.init(token, .{ .If = null });
    }
    if (std.mem.eql(u8, token.word, "else")) {
        return Op.init(token, .{ .Else = null });
    }
    if (std.mem.eql(u8, token.word, "end")) {
        return Op.init(token, .{ .End = null });
    }
    if (std.mem.eql(u8, token.word, "dup")) {
        return Op.init(token, .Dup);
    }
    if (std.mem.eql(u8, token.word, ">")) {
        return Op.init(token, .Gt);
    }
    if (std.mem.eql(u8, token.word, "while")) {
        return Op.init(token, .While);
    }
    if (std.mem.eql(u8, token.word, "do")) {
        return Op.init(token, .{ .Do = null });
    }
    if (std.mem.eql(u8, token.word, "mem")) {
        return Op.init(token, .Mem);
    }

    const value = std.fmt.parseInt(u64, token.word, 10) catch {
        std.debug.print("{s}:{d}:{d}: Invalid number '{s}'\n", .{ token.filePath, token.row, token.col, token.word });
        std.process.exit(1);
    };
    return Op.init(token, .{ .Push = value });
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

fn loadProgram(comptime Reader: type, filePath: []const u8, allocator: Allocator, reader: Reader) !Program {
    var program = std.ArrayList(Op).init(allocator);
    errdefer program.deinit();

    var lineNumber: u64 = 1;

    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', maxLine)) |rawLine| : (lineNumber += 1) {
        defer allocator.free(rawLine);

        const line = if (std.mem.indexOf(u8, rawLine, "//")) |commentStart|
            rawLine[0..commentStart]
        else
            rawLine;
        var col = findCol(line, 0, isNotSpace);
        while (col < line.len) {
            const colEnd = findCol(line, col, std.ascii.isSpace);
            const token = Token{
                .filePath = try allocator.dupe(u8, filePath),
                .row = lineNumber,
                .col = col + 1,
                .word = try allocator.dupe(u8, line[col..colEnd]),
            };
            try program.append(try parseTokenAsOp(token));
            col = findCol(line, colEnd, isNotSpace);
        }
    }

    return Program{ .ops = program.toOwnedSlice(), .allocator = allocator };
}

pub fn loadProgramFromText(allocator: Allocator, text: []const u8) !Program {
    var stream = std.io.fixedBufferStream(text);
    var streamReader = stream.reader();

    return try loadProgram(@TypeOf(streamReader), "<memory>", allocator, streamReader);
}

pub fn loadProgramFromFile(allocator: Allocator, filePath: []const u8) !Program {
    var program = std.ArrayList(Op).init(allocator);
    errdefer program.deinit();

    const inputFilePath = try toAbsolute(allocator, filePath);
    defer allocator.free(inputFilePath);

    var f = try std.fs.openFileAbsolute(inputFilePath, .{});
    defer f.close();
    const fReader = f.reader();

    return try loadProgram(@TypeOf(fReader), inputFilePath, allocator, fReader);
}
