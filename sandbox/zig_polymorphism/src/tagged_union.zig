const std = @import("std");
const stdout = std.io.getStdOut().writer();

pub const Animal = union(enum) {
    const Self = @This();
    dog: Dog,
    cat: Cat,
    lion: Lion,

    pub fn bark(self: Self) *const []const u8 {
        return switch (self) {
            .dog => |n| n.bark(),
            .cat => |n| n.bark(),
            .lion => |n| n.bark(),
        };
    }
};

pub const Dog = struct {
    const Self = @This();

    const Msg: []const u8 = "wan wan";

    pub fn init() Animal {
        return .{ .dog = .{} };
    }

    pub fn bark(self: Self) *const []const u8 {
        _ = self;
        return &Msg;
    }
};

pub const Cat = struct {
    const Self = @This();
    const Msg: []const u8 = "nyan nyan";

    pub fn init() Animal {
        return .{ .cat = .{} };
    }

    pub fn bark(self: Self) *const []const u8 {
        _ = self;
        return &Msg;
    }
};

pub const Lion = struct {
    const Self = @This();
    const Msg: []const u8 = "gao gao";

    pub fn init() Animal {
        return .{ .lion = .{} };
    }

    pub fn bark(self: Self) *const []const u8 {
        _ = self;
        return &Msg;
    }
};

test "union" {
    const animals = [_]Animal{
        Dog.init(),
        Cat.init(),
        Lion.init(),
    };

    try std.testing.expectEqualStrings(animals[0].bark().*, "wan wan"[0..]);
    try std.testing.expectEqualStrings(animals[1].bark().*, "nyan nyan"[0..]);
    try std.testing.expectEqualStrings(animals[2].bark().*, "gao gao"[0..]);
}
