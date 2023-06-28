const ascii = @import("std").ascii;

pub fn isIsogram(str: []const u8) bool {
    var founds: [26]bool = undefined;
    for (str) |c| {
        if (!ascii.isAlphanumeric(c)) {
            continue;
        }

        const ind = ascii.toLower(c) - 'a';
        if (founds[ind]) {
            return false;
        }
        founds[ind] = true;
    }

    return true;
}
