const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const math = std.math;
const builtin = @import("std").builtin;
const Tuple = std.meta.Tuple;

const InputType = struct {
    symbol: []const u8,
    data_type: type,
    data_length: usize,
};

fn Context(comptime input_types: []const InputType) type {
    comptime var fields: [input_types.len]builtin.Type.StructField = undefined;
    for (0..input_types.len) |i| {
        const name = input_types[i].symbol;
        const n = input_types[i].data_length;
        const T = input_types[i].data_type;
        fields[i] = .{ .name = name, .type = [n]T, .default_value = null, .is_comptime = false, .alignment = 0 };
    }

    return @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = &fields,
            .decls = &[_]builtin.Type.Declaration{},
            .is_tuple = false,
        },
    });
}

const Shape = struct {
    value: []const usize,

    fn size(comptime self: Shape) usize {
        comptime var ret: usize = 1;
        inline for (self.value) |dim| {
            ret *= dim;
        }
        return ret;
    }
};

const Input = struct {
    data_type: type,
    data_shape: Shape,
    symbol: []const u8,

    fn T(comptime self: Input) type {
        return self.data_type;
    }

    fn shape(comptime self: Input) Shape {
        return self.data_shape;
    }

    fn gen(comptime self: Input, comptime ContextType: type) self.Layer(ContextType) {
        return .{};
    }

    fn Layer(comptime self: Input, comptime ContextType: type) type {
        const n = self.shape().size();

        return struct {
            const Self = @This();

            fn forward(inner: Self, context: ContextType) [n]self.T() {
                _ = inner;
                const input_value = @field(context, self.symbol);

                var data: [n]self.T() = undefined;
                inline for (0..n) |i| {
                    data[i] = input_value[i];
                }
                return data;
            }
        };
    }
};

const Tensor = struct {
    data_type: type,
    data_shape: Shape,
    data: [*]const u8,

    fn T(comptime self: Tensor) type {
        return self.data_type;
    }

    fn shape(comptime self: Tensor) Shape {
        return self.data_shape;
    }

    fn gen(comptime self: Tensor, comptime ContextType: type) self.Layer(ContextType) {
        const size = self.shape().size();
        return .{
            .data = @ptrCast(*const [size]self.T(), @alignCast(@alignOf(self.T()), self.data)),
        };
    }

    fn Layer(comptime self: Tensor, comptime ContextType: type) type {
        const n = self.shape().size();

        return struct {
            const Self = @This();
            data: *const [n]self.T(),

            fn forward(inner: Self, context: ContextType) [n]self.T() {
                _ = context;
                var data: [n]self.T() = undefined;
                for (0..n) |i| {
                    data[i] = inner.data[i];
                }
                return data;
            }
        };
    }
};

const Add = struct {
    lhs: *const Node,
    rhs: *const Node,

    fn T(comptime self: Add) type {
        return self.lhs.T();
    }

    fn shape(comptime self: Add) Shape {
        return self.lhs.shape();
    }

    fn gen(comptime self: Add, comptime ContextType: type) self.Layer(ContextType) {
        return .{
            .lhs = &self.lhs.gen(ContextType),
            .rhs = &self.rhs.gen(ContextType),
        };
    }
    fn Layer(comptime self: Add, comptime ContextType: type) type {
        return struct {
            const Self = @This();
            lhs: *const self.lhs.Layer(ContextType),
            rhs: *const self.rhs.Layer(ContextType),

            fn forward(inner: Self, context: ContextType) [self.shape().size()]self.T() {
                const n = comptime self.shape().size();
                const lhs_data = inner.lhs.forward(context);
                const rhs_data = inner.rhs.forward(context);

                var data: [n]self.T() = undefined;
                inline for (0..n) |i| {
                    data[i] = lhs_data[i] + rhs_data[i];
                }

                return data;
            }
        };
    }
};

const Sub = struct {
    lhs: *const Node,
    rhs: *const Node,

    fn T(comptime self: Sub) type {
        return self.lhs.T();
    }

    fn shape(comptime self: Sub) Shape {
        return self.lhs.shape();
    }

    fn gen(comptime self: Sub, comptime ContextType: type) self.Layer(ContextType) {
        return .{
            .lhs = &self.lhs.gen(),
            .rhs = &self.rhs.gen(),
        };
    }
    fn Layer(comptime self: Sub, comptime ContextType: type) type {
        return struct {
            const Self = @This();
            lhs: *const self.lhs.Layer(),
            rhs: *const self.rhs.Layer(),

            fn forward(inner: Self, context: ContextType) [self.shape().size()]self.T() {
                const n = comptime self.shape().size();
                const lhs_data = inner.lhs.forward(context);
                const rhs_data = inner.rhs.forward(context);

                var data: [n]self.T() = undefined;
                inline for (0..n) |i| {
                    data[i] = lhs_data[i] - rhs_data[i];
                }

                return data;
            }
        };
    }
};

const Dot = struct {
    lhs: *const Node,
    rhs: *const Node,

    fn T(comptime self: Dot) type {
        return self.lhs.T();
    }

    fn shape(comptime self: Dot) Shape {
        const ret = Shape{ .value = &[_]usize{
            self.lhs.shape().value[0],
            self.rhs.shape().value[1],
        } };
        return ret;
    }

    fn gen(comptime self: Dot, comptime ContextType: type) self.Layer(ContextType) {
        return .{
            .lhs = comptime &self.lhs.gen(ContextType),
            .rhs = comptime &self.rhs.gen(ContextType),
        };
    }

    fn Layer(comptime self: Dot, comptime ContextType: type) type {
        return struct {
            const Self = @This();
            const lhs_shape = self.lhs.shape();
            const rhs_shape = self.rhs.shape();
            lhs: *const self.lhs.Layer(ContextType),
            rhs: *const self.rhs.Layer(ContextType),

            fn forward(inner: Self, context: ContextType) [self.shape().size()]self.T() {
                const n = comptime self.shape().size();
                const lhs_data = inner.lhs.forward(context);
                const rhs_data = inner.rhs.forward(context);

                var data: [n]self.T() = undefined;
                for (0..lhs_shape.value[0]) |i| {
                    const lhs_base_ind = i * lhs_shape.value[1];
                    for (0..rhs_shape.value[1]) |j| {
                        const data_ind = i * rhs_shape.value[1] + j;
                        data[data_ind] = 0.0;
                        for (0..lhs_shape.value[1]) |k| {
                            const lhs_ind = lhs_base_ind + k;
                            const rhs_ind = j + k * rhs_shape.value[1];
                            data[data_ind] += lhs_data[lhs_ind] * rhs_data[rhs_ind];
                        }
                    }
                }

                return data;
            }
        };
    }
};

const ReLU = struct {
    in: *const Node,

    fn T(comptime self: ReLU) type {
        return self.in.T();
    }

    fn shape(comptime self: ReLU) Shape {
        return self.in.shape();
    }

    fn gen(comptime self: ReLU, comptime ContextType: type) self.Layer(ContextType) {
        return .{
            .in = &self.in.gen(ContextType),
        };
    }
    fn Layer(comptime self: ReLU, comptime ContextType: type) type {
        return struct {
            const Self = @This();
            in: *const self.in.Layer(ContextType),

            fn forward(inner: Self, context: ContextType) [self.shape().size()]self.T() {
                const n = comptime self.shape().size();
                const in_data = inner.in.forward(context);

                var data: [n]self.T() = undefined;
                for (0..n) |i| {
                    data[i] = if (0 <= in_data[i]) in_data[i] else 0;
                }

                return data;
            }
        };
    }
};

const SoftMax = struct {
    in: *const Node,

    fn T(comptime self: SoftMax) type {
        return self.in.T();
    }

    fn shape(comptime self: SoftMax) Shape {
        return self.in.shape();
    }

    fn gen(comptime self: SoftMax, comptime ContextType: type) self.Layer(ContextType) {
        return .{
            .in = &self.in.gen(ContextType),
        };
    }
    fn Layer(comptime self: SoftMax, comptime ContextType: type) type {
        return struct {
            const Self = @This();
            in: *const self.in.Layer(ContextType),

            fn forward(inner: Self, context: ContextType) [self.shape().size()]self.T() {
                const n = comptime self.shape().size();
                const in_data = inner.in.forward(context);

                var max_value = in_data[0];
                for (1..n) |i| {
                    max_value = @max(max_value, in_data[i]);
                }

                var data: [n]self.T() = undefined;
                var acc_exp: f32 = 0.0;
                for (0..n) |i| {
                    data[i] = math.exp(in_data[i] - max_value);
                    acc_exp += data[i];
                }
                for (0..n) |i| {
                    data[i] /= acc_exp;
                }

                return data;
            }
        };
    }
};

const Node = union(enum) {
    tensor: Tensor,
    add: Add,
    sub: Sub,
    dot: Dot,
    input: Input,
    relu: ReLU,
    softmax: SoftMax,

    fn T(comptime self: Node) type {
        switch (self) {
            .tensor => |node| return node.T(),
            .add => |node| return node.T(),
            .sub => |node| return node.T(),
            .dot => |node| return node.T(),
            .input => |node| return node.T(),
            .relu => |node| return node.T(),
            .softmax => |node| return node.T(),
        }
    }

    fn shape(comptime self: Node) Shape {
        switch (self) {
            .tensor => |node| return node.shape(),
            .add => |node| return node.shape(),
            .sub => |node| return node.shape(),
            .dot => |node| return node.shape(),
            .input => |node| return node.shape(),
            .relu => |node| return node.shape(),
            .softmax => |node| return node.shape(),
        }
    }

    fn gen(comptime self: Node, comptime ContextType: type) self.Layer(ContextType) {
        switch (self) {
            .tensor => |node| return node.gen(ContextType),
            .add => |node| return node.gen(ContextType),
            .sub => |node| return node.gen(ContextType),
            .dot => |node| return node.gen(ContextType),
            .input => |node| return node.gen(ContextType),
            .relu => |node| return node.gen(ContextType),
            .softmax => |node| return node.gen(ContextType),
        }
    }

    fn Layer(comptime self: Node, comptime ContextType: type) type {
        switch (self) {
            .tensor => |node| return node.Layer(ContextType),
            .add => |node| return node.Layer(ContextType),
            .sub => |node| return node.Layer(ContextType),
            .dot => |node| return node.Layer(ContextType),
            .input => |node| return node.Layer(ContextType),
            .relu => |node| return node.Layer(ContextType),
            .softmax => |node| return node.Layer(ContextType),
        }
    }
};

fn input(comptime T: type, comptime symbol: []const u8, comptime shape: []const usize) Node {
    return Node{ .input = .{ .data_type = T, .symbol = symbol, .data_shape = Shape{ .value = shape } } };
}

fn tensor(comptime T: type, comptime data: []const T, comptime shape: []const usize) Node {
    return Node{ .tensor = .{ .data_type = T, .data = @ptrCast([*]const u8, data), .data_shape = Shape{ .value = shape } } };
}

fn add(comptime lhs: Node, comptime rhs: Node) Node {
    assert(lhs.T() == rhs.T());
    assert(mem.eql(usize, lhs.shape().value, rhs.shape().value));

    return Node{ .add = .{ .lhs = &lhs, .rhs = &rhs } };
}

fn sub(comptime lhs: Node, comptime rhs: Node) Node {
    assert(lhs.T() == rhs.T());
    assert(mem.eql(usize, lhs.shape().value, rhs.shape().value));

    return Node{ .sub = .{ .lhs = &lhs, .rhs = &rhs } };
}

fn dot(comptime lhs: Node, comptime rhs: Node) Node {
    assert(lhs.T() == rhs.T());
    assert(lhs.shape().value[1] == rhs.shape().value[0]);

    return Node{ .dot = .{ .lhs = &lhs, .rhs = &rhs } };
}

fn relu(comptime in: Node) Node {
    return Node{ .relu = .{ .in = &in } };
}

fn softmax(comptime in: Node) Node {
    assert(in.shape().value.len <= 2);
    return Node{ .softmax = .{ .in = &in } };
}

fn AffineWeight(comptime out_dim: usize, comptime in_dim: usize) type {
    return struct {
        weight: *const [out_dim * in_dim]f32,
        bias: *const [out_dim]f32,
    };
}

const MNISTWeights = struct {
    fc1: AffineWeight(500, 784),
    fc2: AffineWeight(10, 500),
};

fn load_affine_weight(comptime model: [*]const u8, comptime out_dim: usize, comptime in_dim: usize) struct { AffineWeight(out_dim, in_dim), usize } {
    comptime var offset: usize = 0;

    const weight_ndim = comptime @ptrCast(*const i32, @alignCast(4, model + offset)).*;
    comptime offset += 4;
    assert(weight_ndim == 2);

    const act_in_dim = comptime @ptrCast(*const i32, @alignCast(4, model + offset)).*;
    comptime offset += 4;
    assert(act_in_dim == in_dim);

    const act_out_dim = comptime @ptrCast(*const i32, @alignCast(4, model + offset)).*;
    comptime offset += 4;
    assert(act_out_dim == out_dim);

    const fc1_weight = comptime @ptrCast(*const [out_dim * in_dim]f32, @alignCast(4, model + offset));
    comptime offset += in_dim * out_dim * 4;

    const fc1_bias_dims = comptime @ptrCast(*const i32, @alignCast(4, model + offset)).*;
    comptime offset += 4;
    assert(fc1_bias_dims == 1);

    const act_out_dim2 = comptime @ptrCast(*const i32, @alignCast(4, model + offset)).*;
    comptime offset += 4;
    assert(act_out_dim2 == out_dim);

    const fc1_bias = comptime @ptrCast(*const [out_dim]f32, @alignCast(4, model + offset));
    comptime offset += out_dim * 4;

    return .{ AffineWeight(out_dim, in_dim){
        .weight = fc1_weight,
        .bias = fc1_bias,
    }, offset };
}

fn load_mnist_weights() MNISTWeights {
    const ggml_model = @embedFile("./ggml-model-f32.bin");

    const magic = comptime @ptrCast(*const i32, @alignCast(4, ggml_model)).*;
    assert(magic == 0x67676d6c);

    comptime var offset = 4;
    const fc1 = comptime load_affine_weight(ggml_model[offset..], 500, 784);
    offset += fc1[1];
    const fc2 = comptime load_affine_weight(ggml_model[offset..], 10, 500);

    return .{
        .fc1 = fc1[0],
        .fc2 = fc2[0],
    };
}

fn be_to_le(be: i32) i32 {
    return (be >> 24) | ((be >> 8) & 0xff00) | ((be << 8) & 0xff0000) | (be << 24);
}

fn load_test_images(n: usize) [784]f32 {
    const t10k_images = @embedFile("./t10k-images.idx3-ubyte");

    const magic = comptime @ptrCast(*const i32, @alignCast(4, t10k_images)).*;
    assert(be_to_le(magic) == 0x00000803);

    const images = comptime @ptrCast(*const i32, @alignCast(4, t10k_images[4..])).*;
    _ = images;
    const rows = comptime @ptrCast(*const i32, @alignCast(4, t10k_images[8..])).*;
    assert(be_to_le(rows) == 28);
    const cols = comptime @ptrCast(*const i32, @alignCast(4, t10k_images[12..])).*;
    assert(be_to_le(cols) == 28);

    const ptr = @ptrCast(*const [784]u8, @alignCast(1, t10k_images[16 + n * 784 ..]));
    var ret: [784]f32 = undefined;
    for (0..784) |i| {
        ret[i] = @intToFloat(f32, ptr[i]);
    }
    return ret;
}

fn print_digit(digit: *const [784]f32) void {
    std.debug.print("\n", .{});
    for (0..28) |i| {
        for (0..28) |j| {
            const pixel = digit.*[i * 28 + j];
            if (0.0 < pixel) {
                std.debug.print("*", .{});
            } else {
                std.debug.print("_", .{});
            }
        }
        std.debug.print("\n", .{});
    }
}

pub fn main() !void {
    const input_types = [_]InputType{
        .{
            .symbol = "input_image",
            .data_type = f32,
            .data_length = 784,
        },
    };
    const ContextType = Context(&input_types);

    const mnist_weights = comptime load_mnist_weights();
    const input_image = comptime input(f32, "input_image", &[_]usize{ 784, 1 });
    const fc1_weight = comptime tensor(f32, mnist_weights.fc1.weight, &[_]usize{ 500, 784 });
    const fc1_bias = comptime tensor(f32, mnist_weights.fc1.bias, &[_]usize{ 500, 1 });
    const fc2_weight = comptime tensor(f32, mnist_weights.fc2.weight, &[_]usize{ 10, 500 });
    const fc2_bias = comptime tensor(f32, mnist_weights.fc2.bias, &[_]usize{ 10, 1 });

    const l1 = comptime relu(add(dot(fc1_weight, input_image), fc1_bias));
    const l2 = comptime relu(add(dot(fc2_weight, l1), fc2_bias));
    const out = comptime softmax(l2);
    const classifier = comptime out.gen(ContextType);

    const n = 407;
    const context: ContextType = .{ .input_image = load_test_images(n) };
    print_digit(&context.input_image);

    var vs = classifier.forward(context);
    for (vs, 0..vs.len) |v, i| {
        std.debug.print("{}: {d:.3}\n", .{ i, v });
    }
}
