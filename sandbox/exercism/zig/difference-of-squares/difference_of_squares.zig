pub fn sumOfSquares(number: usize) usize {
    var ret: usize = 0;
    var i: usize = 1;
    while (i <= number) : (i += 1) {
        ret += i * i;
    }
    return ret;
}

pub fn squareOfSum(number: usize) usize {
    var ret: usize = 0;
    var i: usize = 1;
    while (i <= number) : (i += 1) {
        ret += i;
    }
    ret *= ret;
    return ret;
}

pub fn differenceOfSquares(number: usize) usize {
    return squareOfSum(number) - sumOfSquares(number);
}
