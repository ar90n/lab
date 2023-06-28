const ascii = @import("std").ascii;

fn should_reply_sure(s: []const u8) bool {
    var i: usize = s.len - 1;
    while (0 <= i) : (i -= 1) {
        if (!ascii.isWhitespace(s[i])) {
            return s[i] == '?';
        }
    }

    return false;
}

fn should_reply_chill(s: []const u8) bool {
    var ret = true;
    var n: usize = 0;
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        if (!ascii.isAlphabetic(s[i])) {
            continue;
        }
        n += 1;
        ret = ret and ascii.isUpper(s[i]);
    }
    return ret and (0 < n);
}

fn should_reply_calm(s: []const u8) bool {
    return should_reply_sure(s) and should_reply_chill(s);
}

fn should_reply_file(s: []const u8) bool {
    var n: usize = 0;
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        if (ascii.isWhitespace(s[i])) {
            n += 1;
        }
    }

    return n == s.len;
}

pub fn response(s: []const u8) []const u8 {
    if (should_reply_file(s)) {
        return "Fine. Be that way!";
    } else if (should_reply_calm(s)) {
        return "Calm down, I know what I'm doing!";
    } else if (should_reply_sure(s)) {
        return "Sure.";
    } else if (should_reply_chill(s)) {
        return "Whoa, chill out!";
    } else {
        return "Whatever.";
    }
}
