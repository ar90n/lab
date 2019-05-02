#!/usr/bin/env python3
import sys
from collections.abc import Iterable
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
try:
    from math import gcd
except Exception:
    from fractions import gcd
{% if prediction_success %}
{% else %}
readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]
{% endif %}

{% if mod %}
MOD = {{ mod }}  # type: int
{% endif %}
{% if yes_str %}
YES = "{{ yes_str }}"  # type: str
{% endif %}
{% if no_str %}
NO = "{{ no_str }}"  # type: str
{% endif %}

{% if prediction_success %}
def solve({{ formal_arguments }}):
    return 0

{% endif %}

def main():
    {% if prediction_success %}
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    {{ input_part }}
    result = solve({{ actual_arguments }})
    if isinstance(result, Iterable):
        result = '\n'.join([str(v) for v in result])
    {% else %}
    # Failed to predict input format
    result = 0
    {% endif %}
    print(result)

if __name__ == '__main__':
    main()
