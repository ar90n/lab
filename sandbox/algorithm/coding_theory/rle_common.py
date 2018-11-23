from itertools import groupby


def encode(data, hook):
    result = [hook(list(v)) for _, v in groupby(data)]
    result.append(hook(None))
    return sum(result, [])


def decode(data, hook):
    result = [hook(v) for v in data]
    result.append(hook(None))
    return sum(result, [])


def test_rle_common(_encode, _decode):
    data = [ord(a) for a in 'aaabbc\x80\x80\x80ddddedeee\xffe']
    assert data == decode(encode(data, _encode), _decode)
