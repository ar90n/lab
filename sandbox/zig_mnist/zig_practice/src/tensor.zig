const std = @import("std");

fn Tensor(comptime T: type) type {
    return struct {
        data: []const T,
        shape: []const u32,
    };
}

fn tensor(comptime T: type, comptime data: []const T, comptime shape: []const u32) Tensor(T) {
    comptime var total_elements = 1;
    for (shape) |s| {
        total_elements *= s;
    }
    std.debug.assert(data.len == total_elements);

    return .{ .data = data, .shape = shape };
}

fn add(comptime T: type, comptime a: Tensor(T), comptime b: Tensor(T)) Tensor(T) {
    comptime var result: [a.data.len]T = undefined;
    for (0..a.data.len) |i| {
        result[i] = a.data[i] + b.data[i];
    }

    return tensor(T, &result, a.shape);
}

pub fn main() !void {
    const data: []const i32 = &[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    const shape: []const u32 = &[_]u32{ 3, 3 };
    const t0 = comptime tensor(i32, data, shape);
    const t1 = comptime tensor(i32, &[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, &[_]u32{ 3, 3 });
    const t2 = comptime add(i32, t0, t1);

    for (t2.data) |d| {
        std.debug.print("{d} ", .{d});
    }
}
