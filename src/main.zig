const std = @import("std");
const Ast = @import("Ast.zig");

const ilproj_data =
    \\<Project Sdk="Microsoft.Net.Sdk.il/8.0.0">
    \\  <PropertyGroup>
    \\    <OutputType>Exe</OutputType>
    \\    <TargetFramework>net8.0</TargetFramework>
    \\    <PublishAot>true</PublishAot>
    \\  </PropertyGroup>
    \\</Project>
;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len < 3)
        return error.InvalidArguments;

    std.debug.print("input source: {s}\n", .{args[1]});
    std.debug.print("outdir: {s}\n", .{args[2]});

    const input_file = try std.fs.cwd().openFile(args[1], .{});
    defer input_file.close();

    const input_stem = std.fs.path.stem(args[1]);

    var out_dir = try std.fs.cwd().makeOpenPath(args[2], .{});
    defer out_dir.close();

    const ilproj_filename = try std.mem.concat(alloc, u8, &[_][]const u8{ input_stem, ".ilproj" });
    defer alloc.free(ilproj_filename);
    try out_dir.writeFile2(.{ .sub_path = ilproj_filename, .data = ilproj_data });

    const il_filename = try std.mem.concat(alloc, u8, &[_][]const u8{ input_stem, ".il" });
    defer alloc.free(il_filename);
    const il_file = try out_dir.createFile(il_filename, .{});
    defer il_file.close();

    var ast = Ast.init(alloc);
    defer ast.deinit();

    var timer = try std.time.Timer.start();
    const input_reader = input_file.reader();
    while (try input_reader.readUntilDelimiterOrEofAlloc(alloc, '\n', 10_000)) |line| {
        defer alloc.free(line);
        try ast.parse_line(line);
    }
    const elapsed_ns = timer.read();
    std.log.info("compiled in {d}s", .{@as(f64, @floatFromInt(elapsed_ns)) / std.time.ns_per_s});

    const il_writer = il_file.writer();
    try il_writer.print(".assembly '{s}' {{}}\n", .{input_stem});
    try ast.dump(il_writer);

    std.debug.print("to build for macos:\n\tdotnet publish -r osx-arm64 \"{s}/{s}\"\n", .{ args[2], ilproj_filename });
    std.debug.print("to run on .NET runtime:\n\tdotnet run --project \"{s}/{s}\"\n", .{ args[2], ilproj_filename });
}
