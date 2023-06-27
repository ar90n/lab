const std = @import("std");

const Animal = struct {
    const Self = @This();

    barkFn: fn (self: *const @This()) *const []const u8,

    pub fn init(comptime barkFn: fn (self: *const @This()) *const []const u8) Animal {
        return .{ .barkFn = barkFn };
    }

    pub fn bark(comptime self: *const Self) *const []const u8 {
        return self.barkFn(self);
    }
};

pub const Dog = struct {
    const Self = @This();
    const Msg: []const u8 = "wan wan";

    animal: Animal,
    name: []const u8,

    pub fn init(comptime name: []const u8) Dog {
        return .{
            .animal = Animal.init(Dog.bark),
            .name = name[0..],
        };
    }

    pub fn interface(comptime self: *const Self) *const Animal {
        return &self.animal;
    }

    pub fn bark(comptime animal: *const Animal) *const []const u8 {
        const self = @fieldParentPtr(Self, "animal", animal);
        return &self.name;
    }
};

pub const Cat = struct {
    const Self = @This();
    const Msg: []const u8 = "nyan nyan";

    animal: Animal,
    name: []const u8,

    pub fn init(comptime name: []const u8) Cat {
        return .{
            .animal = Animal.init(Dog.bark),
            .name = name[0..],
        };
    }

    pub fn interface(comptime self: *const Self) *const Animal {
        return &self.animal;
    }

    pub fn bark(comptime animal: *const Animal) *const []const u8 {
        const self = @fieldParentPtr(Self, "animal", animal);
        return &self.name;
    }
};

pub const Lion = struct {
    const Self = @This();
    const Msg: []const u8 = "gao gao";

    animal: Animal,
    name: []const u8,

    pub fn init(comptime name: []const u8) Cat {
        return .{
            .animal = Animal.init(Dog.bark),
            .name = name[0..],
        };
    }

    pub fn interface(comptime self: *const Self) *const Animal {
        return &self.animal;
    }

    pub fn bark(comptime animal: *Animal) *const []const u8 {
        const self = @fieldParentPtr(Self, "animal", animal);
        return &self.name;
    }
};

test "union" {
    const dog = Dog.init("pochi");
    const cat = Cat.init("tama");
    const lion = Lion.init("simba");
    const animals = [_]*const Animal{
        dog.interface(),
        cat.interface(),
        lion.interface(),
    };

    try std.testing.expectEqualStrings(animals[0].bark().*, "pochi"[0..]);
    try std.testing.expectEqualStrings(animals[1].bark().*, "tama"[0..]);
    try std.testing.expectEqualStrings(animals[2].bark().*, "simba"[0..]);
}
