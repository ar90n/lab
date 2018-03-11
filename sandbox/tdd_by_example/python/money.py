#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class Money:

    def __init__(self, amount, currency):
        self._amount = amount
        self.currency = currency

    def __eq__(self, money):
        return self._amount == money._amount \
            and self.currency == money.currency

    def times(self, multiplier):
        return Money(self._amount * multiplier, self.currency)

    @classmethod
    def dollar(cls, amount):
        return Money(amount, 'USD')

    @classmethod
    def franc(cls, amount):
        return Money(amount, 'CHF')


def main():
    pass


if __name__ == '__main__':
    main()
