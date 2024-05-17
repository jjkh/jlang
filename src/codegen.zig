const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Token = @import("tokenizer.zig").Token;
const Type = @import("ast.zig").Type;
const Expression = @import("ast.zig").Expression;
const AstLiteral = @import("ast.zig").AstLiteral;
const builtin_types = @import("ast.zig").builtin_types;

const StaticMethod = struct {
    name: []const u8,
    class: []const u8,
    assembly: []const u8,
    signatures: []const Signature,

    pub const Signature = struct {
        ret_type: Type,
        args: []const Type,
        varargs: ?Type = null,

        pub fn matches(self: Signature, args: []const AstLiteral) bool {
            if (args.len < self.args.len)
                return false;

            // TODO handle inheritance
            for (self.args, 0..) |ty, i| {
                if (!std.mem.eql(u8, ty.clr_name.?, args[i].ty.clr_name.?))
                    return false;
                if (!std.mem.eql(u8, ty.assembly.?, args[i].ty.assembly.?))
                    return false;
            }

            if (args.len > self.args.len) {
                if (self.varargs) |vararg_ty| {
                    // TODO handle inheritance properly
                    if (!std.mem.eql(u8, vararg_ty.name, "object")) {
                        for (args[self.args.len..]) |lit| {
                            if (std.mem.eql(u8, vararg_ty.clr_name.?, lit.ty.clr_name.?))
                                return false;
                            if (!std.mem.eql(u8, vararg_ty.assembly.?, lit.ty.assembly.?))
                                return false;
                        }
                    }
                } else return false;
            }

            return true;
        }
    };

    pub fn get_matching_overload(self: StaticMethod, args: []const AstLiteral) ?Signature {
        for (self.signatures) |sig|
            if (sig.matches(args)) return sig;
        return null;
    }
};

const builtin_funcs = std.StaticStringMap(StaticMethod).initComptime(.{
    .{ "prin", StaticMethod{
        .name = "Write",
        .class = "System.Console",
        .assembly = "mscorlib",
        .signatures = &[_]StaticMethod.Signature{
            .{
                .ret_type = builtin_types.get("void").?,
                .args = &[_]Type{builtin_types.get("int").?},
            },
            .{
                .ret_type = builtin_types.get("void").?,
                .args = &[_]Type{builtin_types.get("object").?},
            },
            .{
                .ret_type = builtin_types.get("void").?,
                .args = &[_]Type{builtin_types.get("string").?},
                .varargs = builtin_types.get("object").?,
            },
        },
    } },
    .{ "print", .{
        .name = "WriteLine",
        .class = "System.Console",
        .assembly = "mscorlib",
        .signatures = &[_]StaticMethod.Signature{
            .{
                .ret_type = builtin_types.get("void").?,
                .args = &[_]Type{builtin_types.get("int").?},
            },
            .{
                .ret_type = builtin_types.get("void").?,
                .args = &[_]Type{builtin_types.get("object").?},
            },
            .{
                .ret_type = builtin_types.get("void").?,
                .args = &[_]Type{builtin_types.get("string").?},
                .varargs = builtin_types.get("object").?,
            },
        },
    } },
});

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
                \\.assembly extern System.Runtime {}
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
                .int32 => |i| if (i >= 0 and i < 9)
                    try self.writer.print("ldc.i4.{}\n", .{i})
                else
                    try self.writer.print("ldc.i4.s {}\n", .{i}),
                .string => |s| try self.writer.print("ldstr \"{s}\"\n", .{s}),
            }
        }

        pub fn writeExpr(self: *Self, expr: Expression) !void {
            switch (expr) {
                .call => |c| {
                    // find matching function
                    const method = builtin_funcs.get(c.func).?;
                    const sig = method.get_matching_overload(c.args).?;

                    // load all args onto the stack
                    for (c.args) |arg| {
                        try self.loadLiteral(arg.value);
                        if (arg.ty.base_type != .reference) {
                            try self.indent();
                            try self.writer.print("box [{s}]{s}\n", .{ arg.ty.assembly.?, arg.ty.clr_name.? });
                        }
                    }

                    // call func
                    try self.indent();
                    try self.writer.print("call {s} [{s}]{s}::{s}(", .{ sig.ret_type.name, method.assembly, method.class, method.name });
                    for (0.., c.args) |i, _| {
                        if (i < sig.args.len)
                            try self.writer.writeAll(sig.args[i].name)
                        else
                            try self.writer.writeAll(sig.varargs.?.name);

                        if (i < c.args.len - 1)
                            try self.writer.writeAll(", ");
                    }
                    try self.writer.writeAll(")\n");
                },
                .binding => |b| {
                    _ = b; // autofix

                },
                .assignment => |a| {
                    _ = a; // autofix

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
        const expr1 = Expression{
            .call = .{
                .func = "print",
                .args = &[_]AstLiteral{
                    .{ .ty = builtin_types.get("string").?, .value = .{ .string = "Hello World! {0}" } },
                    .{ .ty = builtin_types.get("int").?, .value = .{ .int32 = 122 } },
                },
            },
        };
        try cg.writeExpr(expr1);
    }
    try std.testing.expectEqualStrings(
        \\.assembly extern mscorlib {}
        \\.assembly extern System.Runtime {}
        \\
        \\.class private auto ansi beforefieldinit abstract sealed Program extends [mscorlib]System.Object {
        \\    .method private hidebysig static void Main (string[] args) cil managed {
        \\        .entrypoint
        \\        ldstr "Hello World! {0}"
        \\        ldc.i4.s 122
        \\        box [System.Runtime]System.Int32
        \\        call void [mscorlib]System.Console::WriteLine(string, object)
        \\        ret
        \\    }
        \\}
        \\
    , output.constSlice());
}
