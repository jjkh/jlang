const std = @import("std");
const Self = @This();

alloc: std.mem.Allocator,
expressions: std.ArrayList(Expression),
lines: std.ArrayList([]const u8),

var tab_depth: u8 = 0;

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

const Token = union(Type) {
    void: void,
    string: []const u8,

    pub fn managed_load(self: Token, writer: anytype) !void {
        switch (self) {
            .void => return error.CannotLoadVoid,
            .string => |str| try writer.print("ldstr \"{s}\"\n", .{str}),
        }
    }
};
const Expression = union(enum) {
    clr_call: struct { func: []const u8, args: []const Token, ret_type: Type },

    pub fn dump(self: Expression, writer: anytype) !void {
        switch (self) {
            .clr_call => |e| {
                for (e.args) |arg| {
                    try indent(writer);
                    try arg.managed_load(writer);
                }
                try indent(writer);
                try writer.print("call {s} {s}(", .{ e.ret_type.managed_type(), e.func });
                for (0.., e.args) |i, arg| {
                    try writer.writeAll(@as(Type, arg).managed_type());
                    if (i < (e.args.len - 1))
                        try writer.writeAll(", ");
                }
                try writer.writeAll(")\n");
            },
        }
    }

    pub fn free(self: Expression, alloc: std.mem.Allocator) void {
        switch (self) {
            .clr_call => |e| alloc.free(e.args),
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
    buf: []const u8,
    idx: usize = 0,

    fn peek(self: ExpressionParser) ?u8 {
        if (self.idx < self.buf.len - 1)
            return self.buf[self.idx + 1]
        else
            return null;
    }

    fn consume(self: *ExpressionParser) ?u8 {
        const char = self.peek() orelse return null;
        self.idx += 1;
        return char;
    }

    fn consume_one(self: *ExpressionParser, char: u8) !void {
        const new_char = self.consume() orelse return error.EndOfBuffer;
        if (new_char != char)
            return error.UnexpectedChar;
    }

    fn consume_while(self: *ExpressionParser, char: u8) void {
        while (self.consume()) |next_char| {
            if (next_char != char) {
                self.idx -= 1;
                break;
            }
        }
    }

    fn consume_until(self: *ExpressionParser, char: u8) !void {
        while (self.consume()) |next_char| {
            if (next_char == char)
                return;
        }
        return error.EndOfBuffer;
    }

    fn read_string(self: *ExpressionParser) ![]const u8 {
        try self.consume_one('"');

        const start_idx = self.idx + 1; // why?
        try self.consume_until('"');
        return self.buf[start_idx..self.idx];
    }

    fn read_identifier(self: *ExpressionParser) ![]const u8 {
        const start_idx = self.idx;
        try self.consume_until(' ');
        return self.buf[start_idx..self.idx];
    }

    fn read_token(self: *ExpressionParser) !Token {
        if (self.peek()) |next_char| {
            if (next_char == '"')
                return Token{ .string = try self.read_string() }
            else
                return error.InvalidCharForToken;
        }
        return error.EndOfBuffer;
    }

    pub fn parse(self: *ExpressionParser, alloc: std.mem.Allocator) !Expression {
        const identifier = try self.read_identifier();
        self.consume_while(' ');
        const tokens = try alloc.alloc(Token, 1);
        errdefer alloc.free(tokens);
        tokens[0] = try self.read_token();
        if (self.peek() != null) {
            return error.NotImplemented;
        }

        // TODO: all of this
        return Expression{ .clr_call = .{
            .func = clr_mapping.get(identifier) orelse return error.InvalidClrMethod,
            .args = tokens,
            .ret_type = .void,
        } };
    }
};

test "ExpressionParser" {
    var parser = ExpressionParser{ .buf = "print \"Hello World!\"" };
    const expr = try parser.parse();

    var buf: [2000]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try expr.dump(fbs.writer());

    try std.testing.expectEqualStrings(
        \\ldstr "Hello World"
        \\call void [mscorlib]System.Console::WriteLine(string)
        \\
    , fbs.getWritten());
}

pub fn parse_line(self: *Self, line: []const u8) !void {
    try self.lines.append(try self.alloc.dupe(u8, line));
    var parser = ExpressionParser{ .buf = self.lines.getLast() };
    try self.expressions.append(try parser.parse(self.alloc));
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

    try ast.expressions.append(.{
        .clr_call = .{
            .func = "[mscorlib]System.Console::WriteLine",
            .args = &[_]Token{.{ .string = "Hello World!" }},
            .ret_type = .void,
        },
    });

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
