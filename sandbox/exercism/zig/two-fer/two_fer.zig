const fmt = @import("std").fmt;

pub fn twoFer(buffer: []u8, name: ?[]const u8) anyerror![]u8 {
    const name2: []const u8 = name orelse "you";
    return fmt.bufPrint(buffer, "One for {s}, one for me.", .{name2});
}
