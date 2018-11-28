from common import create_converter, test_common


def create_encoder(_make_tree, _encode):
    tree = None

    def _encode_with_tree(x):
        nonlocal tree
        return _encode(tree, x)

    encoder = create_converter(_encode_with_tree)

    def _wrappeed(vs):
        nonlocal tree
        enc_tree, dec_tree = _make_tree(vs)
        tree = enc_tree
        return dec_tree, encoder(vs)

    return _wrappeed


def create_decoder(_decode):
    tree = None

    def _decode_with_tree(x):
        nonlocal tree
        return _decode(tree, x)

    decoder = create_converter(_decode_with_tree)

    def _wrappeed(data):
        nonlocal tree
        tree = data[0]
        x = data[1]
        return decoder(x)

    return _wrappeed


def test_huffman_common(_make_tree, _encode, _decode):
    encoder = create_encoder(_make_tree, _encode)
    decoder = create_decoder(_decode)
    test_common(encoder, decoder)
