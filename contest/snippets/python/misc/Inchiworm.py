def inchworm(sweep_range, f, g):
    beg, end = sweep_range
    left = beg
    right = beg

    while right < end:
        while right < end and f(left, right):
            right += 1

        while left < right and g(left, right):
            left += 1
