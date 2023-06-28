pub const Coordinate = struct {
    // This struct, as well as its fields and methods, needs to be implemented.
    x_coord: f32,
    y_coord: f32,

    pub fn init(x_coord: f32, y_coord: f32) Coordinate {
        return .{
            .x_coord = x_coord,
            .y_coord = y_coord,
        };
    }
    pub fn score(self: Coordinate) usize {
        const sq_dist = self.x_coord * self.x_coord + self.y_coord * self.y_coord;
        if (sq_dist <= 1) {
            return 10;
        } else if (sq_dist <= 5 * 5) {
            return 5;
        } else if (sq_dist <= 10 * 10) {
            return 1;
        } else {
            return 0;
        }
    }
};
