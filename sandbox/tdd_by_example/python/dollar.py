#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class Dollar:

    def __init__(self, amount):
        self.amount = amount
        pass

    def times(self, multiplier):
        return Dollar(self.amount * multiplier)

    def __eq__(self, object):
        return self.amount == object.amount


def main():
    pass


if __name__ == '__main__':
    main()
