const std = @import("std");

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!
}

test "simple test" {
    var r = comptime add(3, 5);

    var rr = comptime blk: {
        const x = 3;
        const y = 5;
        break :blk add(x, y);
    };

    try std.testing.expectEqual(@as(i32, 8), r);
    try std.testing.expectEqual(@as(i32, 8), rr);
}

test "branching on types" {
    const a = 5;
    const b: if (a < 10) f32 else i32 = 5.1;
    try std.testing.expectEqual(@as(f32, 5.1), b);
}

fn Matrix(comptime T: type, comptime width: comptime_int, comptime height: comptime_int) type {
    return [height][width]T;
}

test "returning a type" {
    try std.testing.expect(Matrix(f32, 4, 4) == [4][4]f32);
    try std.testing.expect(Matrix(i32, 4, 8) == [8][4]i32);
}

fn addSmallInts(comptime T: type, a: T, b: T) T {
    return switch (@typeInfo(T)) {
        .ComptimeInt => a + b,
        .Int => |info| if (info.bits <= 16)
            a + b
        else
            @compileError("ints too large"),
        else => @compileError("only ints acceptd"),
    };
}

test "typeinfo switch" {
    const x = comptime addSmallInts(u16, 20, 30);
    try std.testing.expect(@TypeOf(x) == u16);
    try std.testing.expect(x == 50);
    const y = addSmallInts(comptime_int, 20, 30);
    _ = y;
}

fn Vec(
    comptime count: comptime_int,
    comptime T: type,
) type {
    return struct {
        data: [count]T,
        const Self = @This();

        fn abs(self: Self) Self {
            var tmp = Self{ .data = undefined };
            for (self.data, 0..) |elem, i| {
                tmp.data[i] = if (elem < 0) -elem else elem;
            }
            return tmp;
        }

        fn init(data: [count]T) Self {
            return Self{ .data = data };
        }
    };
}

test "generic vector" {
    const x = Vec(3, f32).init([_]f32{ 10, -10, 5 });
    const y = x.abs();
    try std.testing.expect(std.mem.eql(f32, &y.data, &[_]f32{ 10, 10, 5 }));
}

fn Value(comptime T: type) type {
    return struct {
        value: T,
        const Self = @This();

        fn eval(self: Self) T {
            return self.value;
        }
    };
}

fn Add(comptime T: type, comptime L: type, comptime R: type) type {
    return struct {
        left: L,
        right: R,
        const Self = @This();

        fn eval(self: Self) T {
            _ = L.Self;
            return self.left.eval() + self.right.eval();
        }
    };
}

test "value" {
    const x = Value(f32){ .value = 5.0 };
    try std.testing.expect(x.eval() == 5.0);
}

test "add" {
    const x = Value(f32){ .value = 5.0 };
    const y = Value(f32){ .value = 8 };
    const add_op = Add(f32, Value(f32), Value(f32)){ .left = x, .right = y };
    try std.testing.expect(add_op.eval() == 13.0);
}

fn Add2(comptime T: type) type {
    return struct {
        const Self = @This();
        fn gen() fn (T, T) T {
            var b = struct {
                fn call(a: T, b: T) T {
                    return a + b;
                }
            }.call;
            return b;
        }
    };
}

test "add22" {
    const add_op = Add2(f32);
    const f = add_op.gen();
    try std.testing.expect(f(5.0, 8.0) == 13.0);
}

fn Add3(comptime T: type, L: anytype, R: anytype) type {
    return struct {
        left: L,
        right: R,
        const Self = @This();
        const Ret = T;
        fn gen() fn (T, T) T {
            var b = struct {
                fn call(a: T, b: T) T {
                    return a + b;
                }
            }.call;
            return b;
        }
    };
}

test "add2" {
    const add_op = Add2(f32);
    const f = add_op.gen();
    try std.testing.expect(f(5.0, 8.0) == 13.0);
}

fn Variable(comptime v: anytype) type {
    return struct {
        value: @TypeOf(v),
        const Self = @This();
    };
}

test "variable" {
    const x = Variable(5){ .value = 5 };
    try std.testing.expect(x.value == 5);
}

//fn AddOp(comptime T: type) type {
//    return struct {
//        const Self = @This();
//        fn gen() fn (T, T) T {
//            var b = struct {
//                fn call(a: T, b: T) T {
//                    return a + b;
//                }
//            }.call;
//            return b;
//        }
//    };
//}
//
