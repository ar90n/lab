const std = @import("std");
const mem = std.mem;

pub fn isBalanced(allocator: mem.Allocator, s: []const u8) !bool {
    var stack = std.ArrayList(u8).init(allocator);
    defer stack.deinit();

    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        const c = s[i];
        if (c == ')' or c == ']' or c == '}') {
            if (stack.popOrNull()) |top| {
                if ((c == ')' and top != '(') or (c == ']' and top != '[') or (c == '}' and top != '{')) {
                    return false;
                }
            } else {
                return false;
            }
        } else if ((c == '(' or c == '[' or c == '{')) {
            stack.append(c) catch {
                return false;
            };
        }
    }

    return stack.items.len == 0;
}
