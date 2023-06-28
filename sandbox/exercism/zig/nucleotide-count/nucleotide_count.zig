pub const NucleotideError = error{Invalid};

pub const Counts = struct {
    a: u32,
    c: u32,
    g: u32,
    t: u32,
};

pub fn countNucleotides(s: []const u8) NucleotideError!Counts {
    var ret = Counts{
        .a = 0,
        .c = 0,
        .g = 0,
        .t = 0,
    };
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        switch (s[i]) {
            'A' => ret.a += 1,
            'C' => ret.c += 1,
            'G' => ret.g += 1,
            'T' => ret.t += 1,
            else => return NucleotideError.Invalid,
        }
    }

    return ret;
}
