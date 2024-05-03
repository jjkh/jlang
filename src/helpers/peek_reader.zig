const std = @import("std");
const io = std.io;
const testing = std.testing;

pub fn PeekReader(comptime ReaderType: type, comptime max_peek_len: usize) type {
    return struct {
        inner_reader: ReaderType,
        peek_buffer: [max_peek_len]u8 = undefined,
        peek_count: usize = 0, // make this a slice using peek_buffer as a backing field?

        pub const Error = ReaderType.Error;
        pub const PeekError = Error || error{PeekLengthTooLarge};
        pub const Reader = io.Reader(*Self, Error, read);

        const Self = @This();

        pub fn read(self: *Self, dest: []u8) Error!usize {
            // update peeked bytes
            const peeked_bytes_copied = @min(self.peek_count, dest.len);
            @memcpy(dest[0..peeked_bytes_copied], self.peek_buffer[0..peeked_bytes_copied]);
            self.peek_count -= peeked_bytes_copied;
            std.mem.copyForwards(u8, &self.peek_buffer, self.peek_buffer[peeked_bytes_copied..]);

            // do real read
            return peeked_bytes_copied + try self.inner_reader.read(dest[peeked_bytes_copied..]);
        }

        pub fn peekByte(self: *Self) PeekError!?u8 {
            return if (try self.peek(1)) |x| x[0] else null;
        }

        pub fn peek(self: *Self, bytes_to_peek: usize) PeekError!?[]const u8 {
            if (bytes_to_peek > max_peek_len) return error.PeekLengthTooLarge;
            if (bytes_to_peek > self.peek_count)
                self.peek_count += try self.inner_reader.readAll(self.peek_buffer[0..bytes_to_peek][self.peek_count..]);

            if (self.peek_count < bytes_to_peek)
                return null
            else
                return self.peek_buffer[0..bytes_to_peek];
        }

        pub fn reader(self: *Self) Reader {
            return .{ .context = self };
        }
    };
}

/// Returns an initialised `PeekReader`.
pub fn peekReader(inner_reader: anytype, comptime max_peek_len: u64) PeekReader(@TypeOf(inner_reader), max_peek_len) {
    return .{ .inner_reader = inner_reader };
}

test "peekByte" {
    const data = "hello world";
    var fbs = std.io.fixedBufferStream(data);
    var peeker = peekReader(fbs.reader(), 3);

    try testing.expectEqual('h', (try peeker.peekByte()).?);
    try testing.expectEqual('h', (try peeker.peekByte()).?);
    try testing.expectEqual('h', try peeker.reader().readByte());
    try testing.expectEqual('e', (try peeker.peekByte()).?);
    try testing.expectEqual('e', (try peeker.peekByte()).?);
    try testing.expectEqual('e', try peeker.reader().readByte());
}

test "peek" {
    const data = "hello world";
    var fbs = std.io.fixedBufferStream(data);
    var peeker = peekReader(fbs.reader(), 3);

    try testing.expectEqualStrings("he", (try peeker.peek(2)).?);
    try testing.expectEqualStrings("hel", (try peeker.peek(3)).?);
    try testing.expectEqualStrings("he", (try peeker.peek(2)).?);

    try testing.expectEqual('h', try peeker.reader().readByte());
    try testing.expectEqualStrings("el", (try peeker.peek(2)).?);
    try testing.expectEqualStrings("el", (try peeker.peek(2)).?);

    try testing.expectEqualStrings("el", &try peeker.reader().readBytesNoEof(2));
    try testing.expectEqualStrings("lo", (try peeker.peek(2)).?);

    try peeker.reader().skipUntilDelimiterOrEof(0);
    try testing.expectEqual(null, try peeker.peekByte());
}
