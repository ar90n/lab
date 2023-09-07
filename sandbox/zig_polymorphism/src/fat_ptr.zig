const std = @import("std");

const Animal = struct {
    const Self = @This();
    ptr: *anyopaque,
    barkFn: *const fn (self: *const anyopaque) []const u8,

    pub fn bark(self: Self) []const u8 {
        return self.barkFn(self.ptr);
    }
};

pub const Dog = struct {
    const Self = @This();

    buffer: [64]u8,
    len: usize,

    pub fn init(name: []const u8) !Dog {
        const dog = blk: {
            var dog = Self{
                .buffer = undefined,
                .len = 0,
            };

            const s = try std.fmt.bufPrint(&dog.buffer, "{s} - {s}", .{ name, "wan wan" });
            dog.len = s.len;
            break :blk dog;
        };

        return dog;
    }

    pub fn interface(self: *Self) Animal {
        return .{
            .ptr = self,
            .barkFn = Dog.bark,
        };
    }

    pub fn bark(ctx: *const anyopaque) []const u8 {
        const self: *const Self = @ptrCast(@alignCast(@constCast(ctx)));
        return self.buffer[0..self.len];
    }
};

pub const Cat = struct {
    const Self = @This();

    buffer: [64]u8,
    len: usize,

    pub fn init(name: []const u8) !Cat {
        const cat = blk: {
            var cat = Self{ .buffer = undefined, .len = 0 };

            const s = try std.fmt.bufPrint(&cat.buffer, "{s} - {s}", .{ name, "nyan nyan" });
            cat.len = s.len;
            break :blk cat;
        };

        return cat;
    }

    pub fn interface(self: *Self) Animal {
        return .{
            .ptr = self,
            .barkFn = Cat.bark,
        };
    }

    pub fn bark(ctx: *const anyopaque) []const u8 {
        const self: *Self = @ptrCast(@alignCast(@constCast(ctx)));
        return self.buffer[0..self.len];
    }
};

test "fat_ptr" {
    var dog = try Dog.init("pochi");
    var cat = try Cat.init("tama");
    var animals = [_]Animal{
        dog.interface(),
        cat.interface(),
    };

    try std.testing.expectEqualStrings("pochi - wan wan", animals[0].bark());
    try std.testing.expectEqualStrings("tama - nyan nyan", animals[1].bark());
}
