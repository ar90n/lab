const std = @import("std");

const Animal = struct {
    const Self = @This();
    ptr: *const anyopaque,
    barkFn: *const fn (self: *const anyopaque) *const []const u8,

    pub fn bark(self: Self) *const []const u8 {
        return self.barkFn(self.ptr);
    }
};

pub const Dog = struct {
    const Self = @This();
    const Msg: []const u8 = "wan wan";

    name: []const u8,

    pub fn init(name: []const u8) Dog {
        return .{
            .name = name[0..],
        };
    }

    pub fn interface(self: *const Self) Animal {
        return .{
            .ptr = self,
            .barkFn = Dog.bark,
        };
    }

    pub fn bark(ctx: *const anyopaque) *const []const u8 {
        const self: *const Self = @ptrCast(@alignCast(@constCast(ctx)));
        return &self.name;
    }
};

pub const Cat = struct {
    const Self = @This();
    const Msg: []const u8 = "nyan nyan";

    name: []const u8,

    pub fn init(name: []const u8) Cat {
        return .{
            .name = name[0..],
        };
    }

    pub fn interface(self: *const Self) Animal {
        return .{
            .ptr = self,
            .barkFn = Cat.bark,
        };
    }

    pub fn bark(ctx: *const anyopaque) *const []const u8 {
        const self: *Self = @ptrCast(@alignCast(@constCast(ctx)));
        return &self.name;
    }
};

pub const Lion = struct {
    const Self = @This();
    const Msg: []const u8 = "gao gao";

    name: []const u8,

    pub fn init(name: []const u8) Cat {
        return .{
            .name = name[0..],
        };
    }

    pub fn interface(self: *const Self) Animal {
        return .{
            .ptr = self,
            .barkFn = Lion.bark,
        };
    }

    pub fn bark(ctx: *const anyopaque) *const []const u8 {
        const self: *Self = @ptrCast(@alignCast(@constCast(ctx)));
        return &self.name;
    }
};

test "union" {
    const dog = Dog.init("pochi");
    const cat = Cat.init("tama");
    const lion = Lion.init("simba");
    const animals = [_]Animal{
        dog.interface(),
        cat.interface(),
        lion.interface(),
    };

    try std.testing.expectEqualStrings(animals[0].bark().*, "pochi"[0..]);
    try std.testing.expectEqualStrings(animals[1].bark().*, "tama"[0..]);
    try std.testing.expectEqualStrings(animals[2].bark().*, "simba"[0..]);
}
