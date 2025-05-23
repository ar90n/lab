const std = @import("std");

const Animal = struct {
    const Self = @This();

    barkFn: fn (self: *const @This()) []const u8,

    pub fn init(comptime barkFn: fn (self: *const @This()) []const u8) Animal {
        return .{ .barkFn = barkFn };
    }

    pub fn bark(comptime self: *const Self) []const u8 {
        return self.barkFn(self);
    }
};

pub const Dog = struct {
    const Self = @This();

    animal: Animal,
    buffer: [64]u8,
    len: usize,

    pub fn init(comptime name: []const u8) !Dog {
        const dog = blk: {
            var dog = Self{
                .animal = Animal.init(Dog.bark),
                .buffer = undefined,
                .len = 0,
            };

            const s = try std.fmt.bufPrint(&dog.buffer, "{s} - {s}", .{ name, "wan wan" });
            dog.len = s.len;
            break :blk dog;
        };

        return dog;
    }

    pub fn interface(comptime self: *const Self) *const Animal {
        return &self.animal;
    }

    pub fn bark(comptime animal: *const Animal) []const u8 {
        const self = @fieldParentPtr(Self, "animal", animal);
        return self.buffer[0..self.len];
    }
};

pub const Cat = struct {
    const Self = @This();

    animal: Animal,
    buffer: [64]u8,
    len: usize,

    pub fn init(comptime name: []const u8) !Cat {
        const cat = blk: {
            var cat = Self{
                .animal = Animal.init(Cat.bark),
                .buffer = undefined,
                .len = 0,
            };

            const s = try std.fmt.bufPrint(&cat.buffer, "{s} - {s}", .{ name, "nyan nyan" });
            cat.len = s.len;
            break :blk cat;
        };

        return cat;
    }

    pub fn interface(comptime self: *const Self) *const Animal {
        return &self.animal;
    }

    pub fn bark(comptime animal: *const Animal) []const u8 {
        const self = @fieldParentPtr(Self, "animal", animal);
        return self.buffer[0..self.len];
    }
};

test "field_parent_ptr" {
    const dog = try Dog.init("pochi");
    const cat = try Cat.init("tama");
    const animals = [_]*const Animal{
        dog.interface(),
        cat.interface(),
    };

    try std.testing.expectEqualStrings("pochi - wan wan", animals[0].bark());
    try std.testing.expectEqualStrings("tama - nyan nyan", animals[1].bark());
}
