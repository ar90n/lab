pub const ComputationError = error{IllegalArgument};

pub fn steps(number: usize) anyerror!usize {
    if (number == 0) {
        return ComputationError.IllegalArgument;
    }
    var cnt: usize = 0;
    var v = number;
    while (v != 1) : (v = if (v % 2 == 0) v / 2 else v * 3 + 1) {
        cnt += 1;
    }

    return cnt;
}
