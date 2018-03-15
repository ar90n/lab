#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class Sum:

    def __init__(self, augend, addend):
        self.augend = augend
        self.addend = addend

    def __add__(self, addend):
        return Sum(self, addend)

    def __mul__(self, multiplier):
        return Sum(self.augend * multiplier, self.addend * multiplier)

    def __rmul__(self, multiplier):
        return self.__mul__(multiplier)

    def reduce(self, bank, to):
        from .money import Money
        amount = self.augend.reduce(bank, to)._amount + self.addend.reduce(bank, to)._amount
        return Money(amount, to)
