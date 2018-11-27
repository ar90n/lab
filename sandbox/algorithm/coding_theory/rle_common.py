from itertools import groupby

from common import create_converter, test_common


def create_encoder(_encode):
    encoder = create_converter(_encode)

    def _wrappeed(x):
        grouped = [list(y[1]) for y in groupby(x)]
        return encoder(grouped)

    return _wrappeed


def test_rle_common(_encode, _decode):
    encoder = create_encoder(_encode)
    decoder = create_converter(_decode)
    test_common(encoder, decoder)
