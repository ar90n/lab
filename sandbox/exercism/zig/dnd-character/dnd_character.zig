const rand = @import("std").rand;
const sort = @import("std").sort;

var rnd = rand.DefaultPrng.init(100);

pub fn modifier(score: i8) i8 {
    return @divFloor(score - 10, 2);
}

pub fn ability() i8 {
    var buf: [4]u8 = undefined;
    rnd.fill(&buf);

    var i: usize = 0;
    while (i < buf.len) : (i += 1) {
        buf[i] = @mod(buf[i], 6) + 1;
    }

    sort.insertion(u8, &buf, {}, sort.asc(u8));
    return @intCast(i8, buf[0] + buf[1] + buf[2]);
}

pub const Character = struct {
    strength: i8,
    dexterity: i8,
    constitution: i8,
    intelligence: i8,
    wisdom: i8,
    charisma: i8,
    hitpoints: i8,

    pub fn init() Character {
        const constitution = ability();
        return .{
            .strength = ability(),
            .dexterity = ability(),
            .constitution = constitution,
            .intelligence = ability(),
            .wisdom = ability(),
            .charisma = ability(),
            .hitpoints = 10 + modifier(constitution),
        };
    }
};
