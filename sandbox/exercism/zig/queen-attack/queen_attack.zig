pub const QueenError = error{
    InitializationFailure,
};

pub const Queen = struct {
    row: i8,
    col: i8,

    pub fn init(row: i8, col: i8) QueenError!Queen {
        if (row < 0 or 7 < row) {
            return QueenError.InitializationFailure;
        }
        if (col < 0 or 7 < col) {
            return QueenError.InitializationFailure;
        }

        return .{ .row = row, .col = col };
    }

    pub fn canAttack(self: Queen, other: Queen) QueenError!bool {
        const drow = self.row - other.row;
        const dcol = self.col - other.col;
        return (drow == 0) or (dcol == 0) or (dcol * dcol == drow * drow);
    }
};
