const std = @import("std");

fn Context() type {
    return struct {};
}

const Shape = struct {
    value: []const usize,

    fn size(comptime self: Shape) usize {
        comptime var ret: usize = 1;
        for (self.value) |dim| {
            ret *= dim;
        }
        return ret;
    }
};

fn Node(comptime T: type) type {
    return union(enum) {
        tensor: Tensor(T),
        add: Add(T),
        sub: Sub(T),

        fn shape(comptime self: Node(T)) Shape {
            switch (self) {
                .tensor => |node| return node.shape,
                .add => |node| return node.shape(),
                .sub => |node| return node.shape(),
            }
        }

        fn gen(comptime self: Node(T)) Layer(T, self.shape().size()) {
            switch (self) {
                .tensor => |node| return .{ .tensor = node.gen() },
                .add => |node| return .{ .add = node.gen() },
                .sub => |node| return .{ .add = node.gen() },
            }
        }
    };
}

fn Layer(comptime T: type, comptime n: usize) type {
    return union(enum) {
        tensor: TensorLayer(T, n),
        add: AddLayer(T, n),
        sub: SubLayer(T, n),

        fn forward(self: Layer(T, n)) [n]T {
            switch (self) {
                .tensor => |layer| return layer.forward(),
                .add => |layer| return layer.forward(),
                .sub => |layer| return layer.forward(),
            }
        }
    };
}

fn Tensor(comptime T: type) type {
    return struct {
        const Self = @This();
        data: []const T,
        shape: Shape,

        fn gen(comptime self: Tensor(T)) TensorLayer(T, self.data.len) {
            return TensorLayer(T, self.data.len){ .data = self.data[0..self.data.len] };
        }
    };
}

fn TensorLayer(comptime T: type, comptime n: comptime_int) type {
    return struct {
        const Self = @This();
        data: *const [n]T,
        fn forward(self: Self) [n]T {
            var data: [n]T = undefined;
            for (0..n) |i| {
                data[i] = self.data[i];
            }
            return data;
        }
    };
}

fn Add(comptime T: type) type {
    return struct {
        const Self = @This();
        lhs: *const Node(T),
        rhs: *const Node(T),

        fn shape(comptime self: Self) Shape {
            const lhs_shape = self.lhs.shape();
            const rhs_shape = self.rhs.shape();
            _ = rhs_shape;

            return lhs_shape;
        }

        fn gen(comptime self: Self) AddLayer(T, self.shape().size()) {
            return AddLayer(T, self.shape().size()){
                .lhs = &self.lhs.gen(),
                .rhs = &self.rhs.gen(),
            };
        }
    };
}

fn AddLayer(comptime T: type, comptime n: usize) type {
    return struct {
        const Self = @This();
        lhs: *const Layer(T, n),
        rhs: *const Layer(T, n),

        fn forward(self: Self) [n]T {
            const lhs_data = self.lhs.forward();
            const rhs_data = self.rhs.forward();

            var data: [n]T = undefined;
            for (0..n) |i| {
                data[i] = lhs_data[i] + rhs_data[i];
            }

            return data;
        }
    };
}

fn Sub(comptime T: type) type {
    return struct {
        const Self = @This();
        lhs: *const Node(T),
        rhs: *const Node(T),

        fn shape(comptime self: Self) Shape {
            const lhs_shape = self.lhs.shape();
            const rhs_shape = self.rhs.shape();
            _ = rhs_shape;

            return lhs_shape;
        }

        fn gen(comptime self: Self) SubLayer(T, self.shape().size()) {
            return SubLayer(T, self.shape().size()){
                .lhs = &self.lhs.gen(),
                .rhs = &self.rhs.gen(),
            };
        }
    };
}

fn SubLayer(comptime T: type, comptime n: usize) type {
    return struct {
        const Self = @This();
        lhs: *const Layer(T, n),
        rhs: *const Layer(T, n),

        fn forward(self: Self) [n]T {
            const lhs_data = self.lhs.forward();
            const rhs_data = self.rhs.forward();

            var data: [n]T = undefined;
            for (0..n) |i| {
                data[i] = lhs_data[i] - rhs_data[i];
            }

            return data;
        }
    };
}

fn tensor(comptime T: type, comptime data: []const T, comptime shape: []const usize) Node(T) {
    return Node(T){ .tensor = .{
        .data = data,
        .shape = Shape{ .value = shape },
    } };
}

fn add(comptime T: type, comptime lhs: Node(T), comptime rhs: Node(T)) Node(T) {
    return Node(T){ .add = .{ .lhs = &lhs, .rhs = &rhs } };
}

fn sub(comptime T: type, comptime lhs: Node(T), comptime rhs: Node(T)) Node(T) {
    return Node(T){ .sub = .{ .lhs = &lhs, .rhs = &rhs } };
}

const d0 = [_]f32{ 1, 2, 3, 4, 5, 6 };
const d1 = [_]f32{ 10, 2, 3, 4, 5, 6 };
const d2 = [_]f32{ 20, 2, 3, 4, 5, 6 };

pub fn main() !void {
    const t0 = comptime tensor(f32, &d0, &[_]usize{6});
    const t1 = comptime tensor(f32, &d1, &[_]usize{6});
    const t2 = comptime tensor(f32, &d2, &[_]usize{6});
    const a0 = comptime add(f32, t0, t1);
    const a1 = comptime add(f32, t2, a0);
    const ls = comptime a1.gen();

    var vs = ls.forward();
    for (vs) |v| {
        std.debug.print("v: {}\n", .{v});
    }

    //const node = comptime add(f32, tensor(f32, &[_]f32{ 1, 2, 3, 4, 5, 6 }, &[_]usize{ 2, 3 }), tensor(f32, &[_]f32{ 1, 2, 3, 4, 5, 6 }, &[_]usize{ 2, 3 }));
    //const node = comptime tensor(f32, &[_]f32{ 1, 2, 3, 4, 5, 6 }, &[_]usize{ 2, 3 });
    //std.debug.print("node: {s}\n", .{node});
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
