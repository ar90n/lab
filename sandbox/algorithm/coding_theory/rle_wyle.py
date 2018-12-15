from common import BitStream
from rle_common import test_rle_common


def _encode():
    bs = BitStream()

    def _calc_wyle_prefix_length(value):
        len = 0
        while 0 < (value >> (len + 2)):
            len += 1
        return len

    def f(v):
        if v is None:
            ret = bs.flush()
            return [] if ret is None else [ret]

        rl = len(v) - 1
        s = v[0]

        prefix_length = _calc_wyle_prefix_length(rl)
        prefix = (1 << prefix_length) - 1
        value_length = prefix_length + 2

        bs.push(prefix, prefix_length)
        bs.push(0, 1)
        bs.push(rl, value_length)
        bs.push(s, 8)

        result = []
        while 8 <= bs.length:
            result.append(bs.pop(8))
        return result

    return f


def _decode():
    bs = BitStream()
    length = None

    def _decode_prefix():
        nonlocal length

        leading_ones = bs.leading_ones()
        if bs.length == leading_ones:
            return [], 0
        bs.pop(leading_ones)

        length = leading_ones + 3
        return [], 1

    def _decode_run_length():
        nonlocal length

        run_length = bs.pop(length)
        if run_length is None:
            return [], 1

        length = run_length + 1
        return [], 2

    def _decode_value():
        nonlocal length

        value = bs.pop(8)
        if value is None:
            return [], 2

        run_length = length
        length = None
        return [value] * run_length, 0

    states = [_decode_prefix, _decode_run_length, _decode_value]
    state = 0

    def f(v):
        nonlocal state

        if v is not None:
            bs.push(v, 8)

        result = []

        while True:
            _result, next_state = states[state]()
            result += _result
            if next_state == state:
                return result
            state = next_state

    return f


def main():
    test_rle_common(_encode(), _decode())


if __name__ == "__main__":
    main()
