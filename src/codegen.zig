const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Token = @import("tokenizer.zig").Token;
const Type = @import("ast.zig").Type;
const Expression = @import("ast.zig").Expression;

pub fn CodeGenerator(comptime WriterType: type) type {
    return struct {
        writer: WriterType,
        tab_depth: usize,

        const Self = @This();

        pub fn init(writer: WriterType) !Self {
            const code_generator = .{
                .writer = writer,
                .tab_depth = 2,
            };
            try writer.writeAll(
                \\.assembly extern mscorlib {}
                \\
                \\.class private auto ansi beforefieldinit abstract sealed Program extends [mscorlib]System.Object {
                \\    .method private hidebysig static void Main (string[] args) cil managed {
                \\        .entrypoint
                \\
            );
            return code_generator;
        }

        fn indent(self: Self) !void {
            try self.writer.writeByteNTimes(' ', self.tab_depth * 4);
        }

        fn loadLiteral(self: Self, literal: Token.Literal) !void {
            try self.indent();
            switch (literal) {
                .void => return error.CannotLoadVoid,
                .int32 => |i| try self.writer.print("ldc.i32.s {}\n", .{i}),
                .string => |s| try self.writer.print("ldstr \"{s}\"\n", .{s}),
            }
        }

        pub fn writeExpr(self: *Self, expr: Expression) !void {
            switch (expr) {
                .call => |c| {
                    // load all args onto the stack
                    for (c.args) |arg|
                        try self.loadLiteral(arg.literal);

                    // call func
                    try self.indent();
                    try self.writer.print("call {s} {s}(", .{ @tagName(c.ret_type), c.func });
                    for (0.., c.args) |i, arg| {
                        try self.writer.writeAll(@tagName(arg.literal));
                        if (i < c.args.len - 1)
                            try self.writer.writeAll(", ");
                    }
                    try self.writer.writeAll(")\n");
                },
            }
        }

        pub fn deinit(self: *Self) void {
            self.writer.writeAll(
                \\        ret
                \\    }
                \\}
                \\
            ) catch {};
            self.tab_depth = 0;
        }
    };
}

pub fn codeGenerator(writer: anytype) !CodeGenerator(@TypeOf(writer)) {
    return CodeGenerator(@TypeOf(writer)).init(writer);
}

test "codegen" {
    var output: std.BoundedArray(u8, 1000) = .{};
    {
        var cg = try codeGenerator(output.writer());
        defer cg.deinit();
        const expr1 = Expression{ .call = .{
            .func = "[mscorlib]System.Console::WriteLine",
            .args = &[_]Token{
                .{ .literal = .{ .string = "Hello World!" } },
            },
        } };
        try cg.writeExpr(expr1);
    }
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
    , output.constSlice());
}
