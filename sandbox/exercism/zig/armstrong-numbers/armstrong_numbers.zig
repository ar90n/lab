const math = @import("std").math;

pub fn isArmstrongNumber(num: u128) bool {
    if (num == 0) {
        return true;
    }
    var n = num;
    const l: u128 = math.log10(num) + 1;

    var sum: u128 = 0;
    while (0 < n) : (n /= 10) {
        sum += math.powi(u128, (n % 10), l) catch {
            return false;
        };
        if (num < sum) {
            return false;
        }
    }

    return num == sum;
}
