#! /usr/bin/env python3
# -*- coding: utf-8 -*-


class Sum:

    def __init__(self, augend, addend):
        self.augend = augend
        self.addend = addend

    def reduce(self, to):
        from .money import Money
        amount = self.augend._amount + self.addend._amount
        return Money(amount, to)
