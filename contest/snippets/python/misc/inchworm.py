def inchworm(sweep_range, f, g):
    end = N
    left = 0
    right = 0
    ret = 0
    acc = 1

    def _cond():
        return acc <= K

    def _update_ret():
        nonlocal ret
        ret = max(ret ,right - left)

    def _r_event(i):
        nonlocal acc
        acc *= s[i]

    def _l_event(i):
        nonlocal acc
        acc /= s[i]

    while min(left, right) < end:
        while right < end and _cond():
            _update_ret()
            _r_event(right)
            right += 1

        if _cond():
            _update_ret()

        _l_event(left)
        left += 1
