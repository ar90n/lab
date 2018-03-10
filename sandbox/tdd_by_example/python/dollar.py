#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .money import Money


class Dollar(Money):

    def __init__(self, amount):
        super().__init__(amount)

    def times(self, multiplier):
        return Dollar(self._amount * multiplier)


def main():
    pass


if __name__ == '__main__':
    main()
