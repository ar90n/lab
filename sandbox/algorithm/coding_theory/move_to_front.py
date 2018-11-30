from common import test_common


def _encode(vs):
    table = list(range(256))

    def _enc(v):
        code = table.index(v)
        del table[code]
        table.insert(0, v)
        return code
    return [_enc(v) for v in vs]



def _decode(vs):
    table = list(range(256))

    def _dec(v):
        code = table[v]
        del table[v]
        table.insert(0, code)
        return code
    return [_dec(v) for v in vs]


def main():
    test_common(_encode, _decode)


if __name__ == "__main__":
    main()
