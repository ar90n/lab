const std = @import("std");
const mem = std.mem;

pub fn abbreviate(allocator: mem.Allocator, words: []const u8) mem.Allocator.Error![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var i: usize = 0;
    while (i < words.len) : (i += 1) {
        while (i < words.len) : (i += 1) {
            if (!std.ascii.isWhitespace(words[i]) and words[i] != '-' and words[i] != '_') {
                break;
            }
        }
        if (i == words.len) {
            break;
        }

        try buffer.append(std.ascii.toUpper(words[i]));
        while (i < words.len) : (i += 1) {
            if (std.ascii.isWhitespace(words[i]) or words[i] == '-' or words[i] == '_') {
                break;
            }
        }
    }

    return buffer.toOwnedSlice();
}
