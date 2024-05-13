const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Token = @import("tokenizer.zig").Token;

pub const Type = struct {
    name: []const u8,
    base_type: union(enum) { void, value, reference: []const u8 } = .void,
    clr_name: ?[]const u8 = null,
    assembly: ?[]const u8 = null,
};

// TODO this is obviously wrong...
pub const AstLiteral = struct {
    value: Token.Literal,
    ty: Type,
};

// TODO allow custom types
pub const builtin_types = std.StaticStringMap(Type).initComptime(.{
    // zig fmt: off
    .{ "void"  , .{ .name = "void"                                                             , .base_type = .void } },
    .{ "object", .{ .name = "object", .clr_name = "System.Object", .assembly = "System.Runtime", .base_type = .{ .reference = "void" },  } },
    .{ "int"   , .{ .name = "int"   , .clr_name = "System.Int32" , .assembly = "System.Runtime", .base_type = .value,  } },
    .{ "string", .{ .name = "string", .clr_name = "System.String", .assembly = "System.Runtime", .base_type = .{ .reference = "object" },  } },
    // zig fmt: on
});

pub const Expression = union(enum) {
    call: Call,

    const Call = struct {
        func: []const u8,
        args: []const AstLiteral = undefined,

        pub fn from_builtin(keyword: Token.Keyword) ?Call {
            return .{ .func = @tagName(keyword)};
        }

        pub fn free(self: Call, alloc: Allocator) void {
            for (self.args) |arg| arg.value.free(alloc);
            alloc.free(self.args);
        }
    };

    pub fn free(self: Expression, alloc: std.mem.Allocator) void {
        switch (self) {
            .call => |c| c.free(alloc),
        }
    }
};

pub fn ExpressionIterator(comptime TokenIteratorType: type) type {
    return struct {
        alloc: Allocator,
        token_it: TokenIteratorType,
        token_fifo: std.fifo.LinearFifo(Token, .{ .Static = 1 }),

        const Self = @This();

        fn takeCall(self: *Self) !?Expression.Call {
            var expr = Expression.Call.from_builtin((try self.getToken()).?.keyword) orelse
                return error.UnexpectedKeyword;

            var args_list = ArrayList(AstLiteral).init(self.alloc);
            errdefer for (args_list.items) |arg| arg.value.free(self.token_it.alloc);
            while (try self.peekToken()) |token| switch (token) {
                .literal => {
                    const tok = (self.getToken() catch unreachable).?;
                    const ty = switch (tok.literal) {
                        .int32 => builtin_types.get("int").?,
                        .string => builtin_types.get("string").?,
                        .void => undefined, // invalid
                    };
                    try args_list.append(.{ .ty = ty, .value = tok.literal });
                },
                .symbol => |s| switch (s) {
                    .semicolon => {
                        _ = self.getToken() catch unreachable;
                        break;
                    },
                    // else => return error.UnexpectedSymbol
                },
                else => return error.UnexpectedToken,
            } else return error.UnexpectedEndOfTokens;

            expr.args = try args_list.toOwnedSlice();
            return expr;
        }

        fn peekToken(self: *Self) !?Token {
            if (self.token_fifo.count > 0)
                return self.token_fifo.peekItem(0);

            const token = (try self.token_it.next()) orelse return null;
            try self.token_fifo.writeItem(token);
            return token;
        }

        fn getToken(self: *Self) !?Token {
            if (self.token_fifo.readItem()) |token|
                return token;

            return try self.token_it.next();
        }

        pub fn next(self: *Self) !?Expression {
            return if (try self.peekToken()) |token| switch (token) {
                .keyword => .{ .call = (try self.takeCall()).? },
                else => error.InvalidFirstToken,
            } else null;
        }
    };
}

pub fn expressionIterator(alloc: Allocator, token_it: anytype) ExpressionIterator(@TypeOf(token_it)) {
    return .{
        .alloc = alloc,
        .token_it = token_it,
        .token_fifo = std.fifo.LinearFifo(Token, .{ .Static = 1 }).init(),
    };
}

const FakeTokenIterator = struct {
    alloc: Allocator,
    token_fifo: std.fifo.LinearFifo(Token, .{ .Static = 100 }),

    pub fn init(alloc: Allocator, tokens: []const Token) FakeTokenIterator {
        var token_it = .{
            .alloc = alloc,
            .token_fifo = std.fifo.LinearFifo(Token, .{ .Static = 100 }).init(),
        };
        token_it.token_fifo.writeAssumeCapacity(tokens);
        return token_it;
    }

    pub fn next(self: *FakeTokenIterator) !?Token {
        return self.token_fifo.readItem();
    }
};

test "ExpressionIterator" {
    const token_it = FakeTokenIterator.init(std.testing.allocator, &[_]Token{
        .{ .keyword = .prin },
        .{ .literal = .{ .string = try std.testing.allocator.dupe(u8, "test ") } },
        .{ .symbol = .semicolon },

        .{ .keyword = .print },
        .{ .literal = .{ .string = try std.testing.allocator.dupe(u8, "{0}!") } },
        .{ .literal = .{ .int32 = 4 } },
        .{ .symbol = .semicolon },
    });
    var expression_it = expressionIterator(std.testing.allocator, token_it);

    {
        const expr = (try expression_it.next()).?;
        defer expr.free(std.testing.allocator);
        try std.testing.expectEqualDeep(Expression{ .call = .{
            .func = "prin",
            .args = &[_]AstLiteral{
                .{ .ty = builtin_types.get("string").?, .value = .{ .string = "test " } }
            },
        } }, expr);
    }
    {
        const expr = (try expression_it.next()).?;
        defer expr.free(std.testing.allocator);
        try std.testing.expectEqualDeep(Expression{ .call = .{
            .func = "print",
            .args = &[_]AstLiteral{
                .{ .ty = builtin_types.get("string").?, .value = .{ .string = "{0}!" } },
                .{ .ty = builtin_types.get("int").?, .value = .{ .int32 = 4 } },
            },
        } }, expr);
    }
    try std.testing.expectEqual(null, try expression_it.next());
}
