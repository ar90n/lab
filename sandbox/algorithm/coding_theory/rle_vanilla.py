from rle_common import encode, decode, test_rle_common


def _encode(v):
    rl = len(v)
    s = v[0]
    return v if rl <= 2 and s < 0x80 else [0x80 + rl, s]


def _decode():
    rl = 0

    def f(v):
        nonlocal rl

        if rl == 0:
            result = int(v < 0x80)
            rl = max(0, v - 0x80)
        else:
            result = rl
            rl = 0
        return [v] * result
    return f


def main():
    test_rle_common(_encode, _decode())


if __name__ == '__main__':
    main()
