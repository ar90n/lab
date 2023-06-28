const std = @import("std");

pub fn primes(buffer: []u32, limit: u32) []u32 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    var sieve = std.ArrayList(bool).init(allocator);
    defer sieve.deinit();

    sieve.resize(limit + 1) catch {
        return &[_]u32{};
    };
    sieve.items[0] = true;
    sieve.items[1] = true;

    var i: usize = 2;
    while (i <= limit) : (i += 1) {
        var j: usize = 2 * i;
        while (j <= limit) : (j += i) {
            sieve.items[j] = true;
        }
    }

    i = 0;
    var j: usize = 0;
    while (i <= limit) : (i += 1) {
        if (!sieve.items[i]) {
            buffer[j] = @intCast(u32, i);
            j += 1;
        }
    }

    return buffer[0..j];
}
