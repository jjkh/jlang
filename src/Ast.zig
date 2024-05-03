const std = @import("std");
const Self = @This();

alloc: std.mem.Allocator,
expressions: std.ArrayList(Expression),
lines: std.ArrayList([]const u8),

var tab_depth: u8 = 0;

const keywords = std.StaticStringMap(void).initComptime(.{
    .{ "prin", {} },
    .{ "print", {} },
});

const clr_mapping = std.StaticStringMap([]const u8).initComptime(.{
    .{ "prin", "[mscorlib]System.Console::Write" },
    .{ "print", "[mscorlib]System.Console::WriteLine" },
});

const Type = enum {
    void,
    string,

    pub fn managed_type(self: Type) []const u8 {
        return switch (self) {
            .void => "void",
            .string => "string",
        };
    }
};

const Value = union(Type) {
    void,
    string: []const u8,

    pub fn init(alloc: std.mem.Allocator, ty: Type, value: anytype) !Value {
        return switch (ty) {
            .void => return error.CannotInitVoid,
            .string => return .{ .string = try alloc.dupe(u8, value) },
        };
    }

    pub fn deinit(self: Value, alloc: std.mem.Allocator) void {
        switch (self) {
            .void => unreachable,
            .string => |s| alloc.free(s),
        }
    }
};

const Literal = struct {
    value: Value,

    pub fn managed_load(self: Literal, writer: anytype) !void {
        switch (self.value) {
            .void => unreachable,
            .string => |s| try writer.print("ldstr \"{s}\"\n", .{s}),
        }
    }
};

const Variable = struct {
    identifier: ?[]const u8,
    value: Value,
};

const Token = union(enum) {
    comment,
    keyword: []const u8,
    literal: Literal,
    // identifier: []const u8,
};

const Expression = union(enum) {
    call: struct { func: []const u8, args: []const Token, ret_type: Type },

    pub fn dump(self: Expression, writer: anytype) !void {
        switch (self) {
            .call => |e| {
                for (e.args) |arg| switch (arg) {
                    .literal => |l| {
                        try indent(writer);
                        try l.managed_load(writer);
                    },
                    else => return error.Unimplemented,
                };
                try indent(writer);

                try writer.print("call {s} {s}(", .{
                    e.ret_type.managed_type(),
                    clr_mapping.get(e.func) orelse e.func,
                });
                for (0.., e.args) |i, arg| switch (arg) {
                    .literal => |l| {
                        try writer.writeAll(@as(Type, l.value).managed_type());
                        if (i < (e.args.len - 1)) try writer.writeAll(", ");
                    },
                    else => return error.Unimplemented,
                };
                try writer.writeAll(")\n");
            },
        }
    }

    pub fn free(self: Expression, alloc: std.mem.Allocator) void {
        switch (self) {
            .call => |e| {
                for (e.args) |arg| switch (arg) {
                    .comment, .keyword => {},
                    .literal => |l| l.value.deinit(alloc),
                };
                alloc.free(e.args);
            },
        }
    }
};

pub fn init(alloc: std.mem.Allocator) Self {
    return .{
        .alloc = alloc,
        .expressions = std.ArrayList(Expression).init(alloc),
        .lines = std.ArrayList([]const u8).init(alloc),
    };
}

pub fn deinit(self: *Self) void {
    for (self.expressions.items) |expr| expr.free(self.alloc);
    self.expressions.deinit();
    for (self.lines.items) |line| self.alloc.free(line);
    self.lines.deinit();
}

const ExpressionParser = struct {
alloc: std.mem.Allocator,
    buf: []const u8,
    next_idx: usize = 0,

    fn peek(self: ExpressionParser) ?u8 {
        if (self.next_idx < self.buf.len)
            return self.buf[self.next_idx]
        else
            return null;
    }

    fn consume(self: *ExpressionParser) ?u8 {
        const char = self.peek() orelse return null;
        self.next_idx += 1;
        return char;
    }

    fn consume_all(self: *ExpressionParser) void {
        self.next_idx = self.buf.len;
    }

    fn consume_one(self: *ExpressionParser, char: u8) !void {
        const new_char = self.consume() orelse return error.EndOfBuffer;
        if (new_char != char)
            return error.UnexpectedChar;
    }

    fn consume_while(self: *ExpressionParser, char: u8) void {
        while (self.consume()) |next_char| {
            if (next_char != char) {
                self.next_idx -= 1;
                break;
            }
        }
    }

    fn consume_until_or_eob(self: *ExpressionParser, term: u8) []const u8 {
        const start = self.next_idx;
        while (self.peek()) |c| {
            if (c == term) break;
            self.next_idx += 1;
        }
        return self.buf[start..self.next_idx];
    }

    fn consume_until(self: *ExpressionParser, term: u8) ![]const u8 {
        const consumed = self.consume_until_or_eob(term);
        if (self.peek() == null) {
            return error.EndOfBuffer;
        }
        return consumed;
    }

    fn consume_string_literal(self: *ExpressionParser) ![]const u8 {
        try self.consume_one('"');
        const str = try self.consume_until('"');
        try self.consume_one('"');
        return str;
    }

    fn read_token(self: *ExpressionParser) !Token {
        if (self.peek()) |next_char| {
            return switch (next_char) {
                '"' => .{
                    .literal = .{
                        .value = try Value.init(self.alloc, .string, try self.consume_string_literal()),
                    },
                },
                '#' => .{ .comment = self.consume_all() },
                'a'...'z', 'A'...'Z', '_' => .{ .keyword = self.consume_until_or_eob(' ') },
                else => error.InvalidFirstCharForToken,
            };
        } else return error.EndOfBuffer;
    }

    pub fn parse(self: *ExpressionParser) !?Expression {
        var tokenList = std.ArrayList(Token).init(self.alloc);
        defer tokenList.deinit();

        while (self.peek() != null) {
            try tokenList.append(try self.read_token());
            // all tokens separated with whitespace (for now)
            self.consume_while(' ');
        }
        if (tokenList.items.len == 0)
            return null; // empty line

        switch (tokenList.items[0]) {
            // if the first token is a comment, ignore the line
            .comment => return null,
            // cannot start an expression with a literal
            .literal => return error.SyntaxError,
            else => {},
        }

        if (keywords.getIndex(tokenList.items[0].keyword)) |kw_i| {
            return .{ .call = .{
                .func = keywords.keys()[kw_i],
                .args = try self.alloc.dupe(Token, tokenList.items[1..]),
                .ret_type = .void,
            } };
        } else {
            // TODO: implement everything else
            return error.Unimplemented;
        }
    }
};

test "ExpressionParser call" {
    var parser = ExpressionParser{ .alloc = std.testing.allocator, .buf = "print \"Hello World!\"" };
    const expr = (try parser.parse()) orelse return error.Fail;
    defer expr.free(std.testing.allocator);

    var buf: [2000]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try expr.dump(fbs.writer());

    try std.testing.expectEqualStrings(
        \\ldstr "Hello World!"
        \\call void [mscorlib]System.Console::WriteLine(string)
        \\
    , fbs.getWritten());
}

test "ExpressionParser comment" {
    var parser = ExpressionParser{ .alloc = std.testing.allocator, .buf = "# print \"Hello World!\"" };
    try std.testing.expectEqual(null, try parser.parse());
}

pub fn parse_line(self: *Self, line: []const u8) !void {
    try self.lines.append(try self.alloc.dupe(u8, line));
    var parser = ExpressionParser{ .alloc = self.alloc, .buf = self.lines.getLast() };
    try self.expressions.append(try parser.parse() orelse return);
}

fn indent(writer: anytype) !void {
    try writer.writeByteNTimes(' ', tab_depth * 4);
}

pub fn dump(self: Self, writer: anytype) !void {
    try writer.writeAll(
        \\.assembly extern mscorlib {}
        \\
        \\.class private auto ansi beforefieldinit abstract sealed Program extends [mscorlib]System.Object {
        \\    .method private hidebysig static void Main (string[] args) cil managed {
        \\        .entrypoint
        \\
    );
    tab_depth = 2;

    for (self.expressions.items) |expr|
        try expr.dump(writer);

    try writer.writeAll(
        \\        ret
        \\    }
        \\}
        \\
    );
    tab_depth = 0;
}

test "clr_mapping" {
    var ast = init(std.testing.allocator);
    defer ast.deinit();

    // TODO: this is a footgun
    var tokens = try std.testing.allocator.alloc(Token, 1);
    tokens[0] = .{ .literal = .{ .value = try Value.init(std.testing.allocator, .string, "Hello World!") } };

    try ast.expressions.append(.{ .call = .{ .func = "print", .args = tokens, .ret_type = .void } });

    var buf: [2000]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try ast.dump(fbs.writer());
    try std.testing.expectEqualStrings(
        \\.assembly extern mscorlib {}
        \\
        \\.class private auto ansi beforefieldinit abstract sealed Program extends [mscorlib]System.Object {
        \\    .method private hidebysig static void Main (string[] args) cil managed {
        \\        .entrypoint
        \\        ldstr "Hello World!"
        \\        call void [mscorlib]System.Console::WriteLine(string)
        \\        ret
        \\    }
        \\}
        \\
    , fbs.getWritten());
}
