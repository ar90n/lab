def _ln2(value):
    ret = 0
    while 0 < value:
        ret += 1
        value = value >> 1
    return ret


class BitStream:
    def __init__(self):
        self._buf = 0
        self.length = 0

    def push(self, value, width=1):
        assert _ln2(value) <= width

        self._buf = (self._buf << width) | value
        self.length += width

    def pop(self, width=8):
        val = self.peek(width)
        if val is not None:
            self.length -= width
            self._buf &= (1 << self.length) - 1
        return val

    def peek(self, width=8):
        if self.length < width:
            return None
        return self._buf >> (self.length - width)

    def flush(self, fill=0):
        if self.length < 8:
            fill = 1 if fill != 0 else 0
            required_bits = 8 - self.length
            self.push(fill, required_bits)
        return self.pop(8)

    def leading_ones(self):
        if self.length == 0:
            return 0

        count = 0
        while 0 < (self._buf & (1 << (self.length - count - 1))):
            count += 1
        return count

    def to_list(self):
        result = []
        while 8 <= self.length:
            result.append(self.pop())
        result.append(self.flush())
        return result


def create_converter(hook):
    def _f(data):
        result = [hook(v) for v in data]
        result.append(hook(None))
        return sum(result, [])

    return _f


def test_common(encoder, decoder):
    data = [ord(a) for a in "aaabbc\x80\x80\x80ddddedeee\xffe"]
    assert data == decoder(encoder(data))
