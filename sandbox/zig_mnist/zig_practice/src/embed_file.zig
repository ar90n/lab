const std = @import("std");

fn reserse(comptime path: []const u8) type {
    return struct {
        const data = @embedFile(path);
        const data_length: usize = data.len;

        pub fn get() *const [data_length:0]u8 {
            var ret: [data_length:0]u8 = undefined;
            for (data, 0..data_length) |c, i| {
                ret[data_length - i - 1] = c;
            }
            return &ret;
        }
    };
}

pub fn main() void {
    std.debug.print("{s}", .{comptime reserse("data.txt").get()});
}
