const std = @import("std");
const ascii = @import("std").ascii;

pub fn isValid(s: []const u8) bool {
    var n: usize = 0;
    var sum: u32 = 0;
    var i: usize = s.len;

    while (0 < i) : (i -= 1) {
        const c = s[i - 1];
        if (ascii.isWhitespace(c)) {
            continue;
        }
        if (c < '0' or '9' < c) {
            return false;
        }

        var v = c - '0';
        if (n % 2 == 1) {
            v *= 2;
        }
        if (9 < v) {
            v -= 9;
        }
        sum += v;
        n += 1;
    }
    return (sum % 10 == 0) and (n > 1);
}
