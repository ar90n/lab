const std = @import("std");
const mem = @import("std").mem;

pub const Signal = enum { wink, double_blink, close_your_eyes, jump };

pub fn calculateHandshake(allocator: mem.Allocator, number: isize) mem.Allocator.Error![]const Signal {
    var buf = std.ArrayList(Signal).init(allocator);
    defer buf.deinit();
    if (number & 0x01 != 0) {
        try buf.append(.wink);
    }
    if (number & 0x02 != 0) {
        try buf.append(.double_blink);
    }
    if (number & 0x04 != 0) {
        try buf.append(.close_your_eyes);
    }
    if (number & 0x08 != 0) {
        try buf.append(.jump);
    }
    if (number & 0x10 != 0) {
        var i: usize = 0;
        while (i < buf.items.len / 2) {
            var tmp = buf.items[i];
            buf.items[i] = buf.items[buf.items.len - i - 1];
            buf.items[buf.items.len - i - 1] = tmp;
            i += 1;
        }
    }

    return buf.toOwnedSlice();
}
