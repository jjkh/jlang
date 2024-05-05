const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

const PeekReader = @import("helpers/peek_reader.zig").PeekReader;
const peekReader = @import("helpers/peek_reader.zig").peekReader;

pub const Type = enum {
    void,
    int,
    string,
};

pub const Token = union(enum) {
    keyword: Keyword,
    literal: Literal,

    pub const Keyword = enum {
        prin,
        print,
    };
    pub const Literal = union(Type) {
        void,
        int: i32,
        string: []const u8,

        pub fn free(self: Literal, alloc: Allocator) void {
            switch (self) {
                .string => alloc.free(self.string),
                else => {},
            }
        }

        pub fn prettyPrint(self: Literal, writer: anytype) !void {
            try switch (self) {
                .void => writer.writeAll("{}"),
                .int => |i| writer.print("{}", .{i}),
                .string => |s| writer.print("\"{s}\"", .{s}),
            };
        }
    };
    const keywords = std.StaticStringMap(Keyword).initComptime(.{
        .{ "prin", .prin },
        .{ "print", .print },
    });

    pub fn free(self: Token, alloc: Allocator) void {
        switch (self) {
            .literal => |l| l.free(alloc),
            else => {},
        }
    }

    pub fn prettyPrint(self: Token, writer: anytype) !void {
        try switch (self) {
            .keyword => |k| writer.print("{s}", .{@tagName(k)}),
            .literal => |l| l.prettyPrint(writer),
        };
    }
};

test "pretty print token" {
    // prepare writer stream
    var buf: [2000]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);

    {
        const keyword = Token{ .keyword = .prin };
        try keyword.prettyPrint(fbs.writer());
        try std.testing.expectEqualStrings("prin", fbs.getWritten());
    }

    fbs.reset();
    {
        const literal = Token{ .literal = .{ .string = "hello there" } };
        try literal.prettyPrint(fbs.writer());
        try std.testing.expectEqualStrings("\"hello there\"", fbs.getWritten());
    }
}

pub fn TokenIterator(comptime ReaderType: type) type {
    return struct {
        peeker: PeekReader(ReaderType, 1),
        alloc: Allocator,

        const Self = @This();

        fn takeStringLiteral(self: *Self) ![]const u8 {
            assert(try self.peeker.reader().readByte() == '"');

            var literal_list = ArrayList(u8).init(self.alloc);
            errdefer literal_list.deinit();

            try self.peeker.reader().streamUntilDelimiter(literal_list.writer(), '"', null);
            return try literal_list.toOwnedSlice();
        }

        fn takeIntLiteral(self: *Self) !i32 {
            var int_bytes: std.BoundedArray(u8, 11) = .{};
            const first_byte = try self.peeker.reader().readByte();
            assert(switch (first_byte) {
                '-', '0'...'9' => true,
                else => false,
            });
            try int_bytes.append(first_byte);

            while (try self.peeker.peekByte()) |c| switch (c) {
                '0'...'9' => try int_bytes.append(try self.peeker.reader().readByte()),
                '_' => try self.peeker.reader().skipBytes(1, .{}),
                'a'...'z', 'A'...'Z', '"' => return error.InvalidIntCharacter,
                else => break,
            };

            return try std.fmt.parseInt(i32, int_bytes.slice(), 10);
        }

        fn takeIdentifier(self: *Self) ![]const u8 {
            var identifier_list = ArrayList(u8).init(self.alloc);
            errdefer identifier_list.deinit();
            const first_byte = try self.peeker.reader().readByte();
            assert(switch (first_byte) {
                '_', 'a'...'z', 'A'...'Z' => true,
                else => false,
            });
            try identifier_list.append(first_byte);

            while (try self.peeker.peekByte()) |c| switch (c) {
                '_',
                '0'...'9',
                'a'...'z',
                'A'...'Z',
                => try identifier_list.append(try self.peeker.reader().readByte()),
                else => break,
            };

            return try identifier_list.toOwnedSlice();
        }

        pub fn next(self: *Self) !?Token {
            while (try self.peeker.peekByte()) |c| switch (c) {
                // skip whitespace
                ' ', '\t', '\r', '\n' => self.peeker.reader().skipBytes(1, .{}),
                // skip comment
                '#' => self.peeker.reader().skipUntilDelimiterOrEof('\n'),
                // proper character
                else => break,
            } catch {};

            return if (try self.peeker.peekByte()) |c| switch (c) {
                '"' => .{ .literal = .{ .string = try self.takeStringLiteral() } },
                '-', '0'...'9' => .{ .literal = .{ .int = try self.takeIntLiteral() } },
                '_', 'a'...'z', 'A'...'Z' => blk: {
                    const identifier = try self.takeIdentifier();
                    errdefer self.alloc.free(identifier);
                    if (Token.keywords.get(identifier)) |keyword| {
                        self.alloc.free(identifier);
                        break :blk .{ .keyword = keyword };
                    } else return error.Unimplemented;
                },
                else => error.InvalidChar,
            } else null;
        }
    };
}

pub fn tokenIterator(alloc: Allocator, reader: anytype) TokenIterator(@TypeOf(reader)) {
    return .{
        .alloc = alloc,
        .peeker = peekReader(reader, 1),
    };
}

test "TokenIterator" {
    var fbs = std.io.fixedBufferStream(
        \\"test" 1# hello there
        \\# comment 12
        \\
        \\-2_345_678 "test 2"
        \\   # "string in comment"
        \\prin "test 3, " # first print
        \\print "test " 4
    );
    var ts = tokenIterator(std.testing.allocator, fbs.reader());

    var token_list = ArrayList(Token).init(std.testing.allocator);
    defer {
        for (token_list.items) |token| token.free(std.testing.allocator);
        token_list.deinit();
    }

    while (try ts.next()) |token|
        try token_list.append(token);

    try std.testing.expectEqualDeep(&[_]Token{
        .{ .literal = .{ .string = "test" } },
        .{ .literal = .{ .int = 1 } },
        .{ .literal = .{ .int = -2345678 } },
        .{ .literal = .{ .string = "test 2" } },
        .{ .keyword = .prin },
        .{ .literal = .{ .string = "test 3, " } },
        .{ .keyword = .print },
        .{ .literal = .{ .string = "test " } },
        .{ .literal = .{ .int = 4 } },
    }, token_list.items);

    try std.testing.expectEqual(null, try ts.next());
}
