#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class Money:

    def __init__(self, amount, currency):
        self._amount = amount
        self.currency = currency

    def __add__(self, addend):
        from .sum import Sum
        return Sum(self, addend)

    def __eq__(self, money):
        return self._amount == money._amount \
            and self.currency == money.currency

    def times(self, multiplier):
        return Money(self._amount * multiplier, self.currency)

    def reduce(self, bank, to):
        rate = bank.rate(self.currency, to)
        return Money(self._amount / rate, to)

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
