const std = @import("std");
const stdout = std.io.getStdOut().writer();

pub const Animal = union(enum) {
    const Self = @This();
    dog: Dog,
    cat: Cat,

    pub fn bark(self: Self) []const u8 {
        return switch (self) {
            inline else => |n| n.bark(),
        };
    }
};

pub const Dog = struct {
    const Self = @This();

    buffer: [64]u8,
    len: usize,

    pub fn init(name: []const u8) !Animal {
        const dog = blk: {
            var dog = Self{
                .buffer = undefined,
                .len = 0,
            };

            const s = try std.fmt.bufPrint(&dog.buffer, "{s} - {s}", .{ name, "wan wan" });
            dog.len = s.len;
            break :blk dog;
        };

        return .{ .dog = dog };
    }

    pub fn bark(self: Self) []const u8 {
        return self.buffer[0..self.len];
    }
};

pub const Cat = struct {
    const Self = @This();

    buffer: [64]u8,
    len: usize,

    pub fn init(name: []const u8) !Animal {
        const cat = blk: {
            var cat = Self{
                .buffer = undefined,
                .len = 0,
            };

            const s = try std.fmt.bufPrint(&cat.buffer, "{s} - {s}", .{ name, "nyan nyan" });
            cat.len = s.len;
            break :blk cat;
        };

        return .{ .cat = cat };
    }

    pub fn bark(self: Self) []const u8 {
        return self.buffer[0..self.len];
    }
};

test "tagged_union" {
    const animals = [_]Animal{
        try Dog.init("pochi"),
        try Cat.init("mike"),
    };

    try std.testing.expectEqualStrings("pochi - wan wan", animals[0].bark());
    try std.testing.expectEqualStrings("mike - nyan nyan", animals[1].bark());
}
