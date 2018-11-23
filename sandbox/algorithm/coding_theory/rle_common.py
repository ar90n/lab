from itertools import groupby


def encode(data, hook):
    return sum([hook(list(v)) for _, v in groupby(data)], [])


def decode(data, hook):
    return sum([hook(v) for v in data], [])


def test_rle_common(_encode, _decode):
    data = [ord(a) for a in 'aaabbc\x80\x80\x80ddddedeee\xffe']
    assert data == decode(encode(data, _encode), _decode)
