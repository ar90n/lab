pub const ChessboardError = error{IndexOutOfBounds};

pub fn square(index: usize) ChessboardError!u64 {
    if (index == 0 or 64 < index) {
        return ChessboardError.IndexOutOfBounds;
    }
    return @as(u64, 1) << @truncate(u6, index - 1);
}

pub fn total() u64 {
    return 0xFFFFFFFFFFFFFFFF;
}
