#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class Money:

    def __init__(self, amount):
        self._amount = amount

    def __eq__(self, object):
        return self._amount == object._amount \
            and type(self) == type(object)

    @classmethod
    def dollar(cls, amount):
        from .dollar import Dollar
        return Dollar(amount)

    @classmethod
    def franc(cls, amount):
        from .franc import Franc
        return Franc(amount)


def main():
    pass


if __name__ == '__main__':
    main()
