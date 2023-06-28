const std = @import("std");

pub fn isValidIsbn10(s: []const u8) bool {
    if (s.len != 13 and s.len != 10) return false;
    var sum: u32 = 0;
    var i: u32 = 0;
    var j: u32 = 0;
    while (i < s.len) : (i += 1) {
        var v = switch (s[i]) {
            '-' => 11,
            'X' => 10,
            else => |vv| vv - '0',
        };
        if (v == 11) continue;
        if (v == 10 and j != 9) return false;
        sum += v * (10 - j);
        j += 1;
    }
    return sum % 11 == 0;
}
