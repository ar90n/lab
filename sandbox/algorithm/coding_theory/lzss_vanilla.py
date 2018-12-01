from collections import deque
from itertools import takewhile

from common import BitStream, test_common

BUFFER_LENGHT = 12
CODE_LENGTH = 4


def match_longest(dic, code):
    longest_match_pos = 0
    longest_match_length = 0
    for i, _ in enumerate(dic):
        match = len(
            list(
                takewhile(lambda x: x == True, [a == b for a, b in zip(dic[i:], code)])
            )
        )
        if longest_match_length <= match:
            longest_match_length = match
            longest_match_pos = i

    longest_match_pos = max(len(dic) - longest_match_pos - 1, 0)
    return longest_match_pos, longest_match_length


def _encode(vs):
    bs = BitStream()

    i = 0
    while i < len(vs):
        code_length = min(len(vs) - 1 - i, CODE_LENGTH)
        buffer_length = min(i, BUFFER_LENGHT)

        code = vs[i : i + code_length]
        buffer = vs[i - buffer_length : i]
        pos, length = match_longest(buffer, code)
        if 2 < length:
            bs.push(1, 1)
            bs.push(pos, 8)
            bs.push(length, 8)
            i += length
        else:
            bs.push(0, 1)
            bs.push(vs[i], 8)
            i += 1

    return bs.to_list()


def _decode(vs):
    bs = BitStream()

    result = []
    for v in vs:
        bs.push(v, 8)

        while True:
            if bs.peek(1) == 1 and 17 < bs.length:
                bs.pop(1)
                beg = -1 - bs.pop(8)
                end = beg + bs.pop(8)
                if end == 0:
                    end = None
                result += result[beg:end]
            elif bs.peek(1) == 0 and 9 < bs.length:
                bs.pop(1)
                result.append(bs.pop(8))
            else:
                break
    return result


def main():
    test_common(_encode, _decode)


if __name__ == "__main__":
    main()
