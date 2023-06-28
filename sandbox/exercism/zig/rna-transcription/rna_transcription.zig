// Import the appropriate standard library and modules
const std = @import("std");
const mem = @import("std").mem;

pub fn toRna(allocator: mem.Allocator, dna: []const u8) mem.Allocator.Error![]const u8 {
    var arr = std.ArrayList(u8).init(allocator);
    defer arr.deinit();

    var i: usize = 0;
    while (i < dna.len) : (i += 1) {
        const c: u8 = switch (dna[i]) {
            'G' => 'C',
            'C' => 'G',
            'T' => 'A',
            else => 'U',
        };
        try arr.append(c);
    }

    return arr.toOwnedSlice();
}
