#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import sys


class IO:

    def __init__(self, effect):
        if not callable(effect):
            raise ValueError('IO Usage: function required')
        self.effect = effect

    @classmethod
    def of(cls, effect):
        return IO(effect)

    def map(self, f):
        def doit():
            return f(self.effect())
        return IO(doit)

    def chain(self, f):
        return f(self.effect())

    def run(self):
        return self.effect()


def main():
    m = IO.of(sys.stdin.readline).map(lambda x: x.upper())
    print(m.run())


if __name__ == '__main__':
    main()
