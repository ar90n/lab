#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class Sum:

    def __init__(self, augend, addend):
        self.augend = augend
        self.addend = addend

    def reduce(self, bank, to):
        from .money import Money
        amount = self.augend.reduce(bank, to)._amount + self.addend.reduce(bank, to)._amount
        return Money(amount, to)
