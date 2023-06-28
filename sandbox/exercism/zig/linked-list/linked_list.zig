pub fn LinkedList(comptime T: type) type {
    return struct {
        const Self = @This();
        // Please implement the doubly linked `Node` (replacing each `void`).
        pub const Node = struct {
            prev: ?*Node = null,
            next: ?*Node = null,
            data: T,
        };

        // Please implement the fields of the linked list (replacing each `void`).
        first: ?*Node = null,
        last: ?*Node = null,
        len: usize = 0,

        // Please implement the below methods.
        // You need to add the parameters to each method.

        pub fn push(self: *Self, n: *Node) void {
            if (self.first == null) {
                self.first = n;
                self.last = n;
                n.*.prev = null;
                n.*.next = null;
            } else {
                n.*.next = self.first;
                n.*.prev = null;
                self.first = n;
                if (n.*.next) |p| {
                    p.*.prev = n;
                }
            }
            self.len += 1;
        }

        pub fn pop(self: *Self) ?*Node {
            if (self.first) |p_first| {
                var n = self.first;
                self.first = p_first.*.next;
                if (self.first) |p_first2| {
                    p_first2.*.prev = null;
                }
                self.len -= 1;
                if (self.len == 0) {
                    self.first = null;
                    self.last = null;
                }
                return n;
            }

            return null;
        }

        pub fn shift(self: *Self) ?*Node {
            if (self.last) |p_last| {
                var n = self.last;
                self.last = p_last.*.prev;
                if (self.last) |p_last2| {
                    p_last2.*.next = null;
                }
                self.len -= 1;
                if (self.len == 0) {
                    self.first = null;
                    self.last = null;
                }
                return n;
            }
            return null;
        }

        pub fn unshift(self: *Self, n: *Node) void {
            if (self.last == null) {
                self.first = n;
                self.last = n;
                n.*.prev = null;
                n.*.next = null;
            } else {
                n.*.next = null;
                n.*.prev = self.last;
                self.last = n;
                if (n.*.prev) |p| {
                    p.*.next = n;
                }
            }
            self.len += 1;
        }

        pub fn delete(self: *Self, n: *Node) void {
            var c = self.first;
            while (c != null) {
                if (c) |cp| {
                    if (cp.*.data == n.*.data) {
                        if (cp.*.prev) |pp| {
                            pp.*.next = cp.*.next;
                        } else {
                            self.first = cp.*.next;
                        }
                        if (cp.*.next) |pn| {
                            pn.*.prev = cp.*.prev;
                        } else {
                            self.last = cp.*.prev;
                        }
                        self.len -= 1;
                        break;
                    }
                    c = cp.*.next;
                }
            }
        }
    };
}
