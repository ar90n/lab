const ascii = @import("std").ascii;

pub fn isPangram(str: []const u8) bool {
    var founds: [26]bool = undefined;
    for (str) |c| {
        if (!ascii.isAlphabetic(c)) {
            continue;
        }
        founds[ascii.toLower(c) - 'a'] = true;
    }

    for (founds) |found| {
        if (!found) {
            return false;
        }
    }

    return true;
}
