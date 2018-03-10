#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class Franc:

    def __init__(self, amount):
        self._amount = amount
        pass

    def times(self, multiplier):
        return Franc(self._amount * multiplier)

    def __eq__(self, object):
        return self._amount == object._amount


def main():
    pass


if __name__ == '__main__':
    main()
