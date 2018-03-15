#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .money import Money


class Bank:

    def __init__(self):
        self.rates = {}

    def reduce(self, source, to):
        return source.reduce(self, to)

    def addRate(self, original, to, rate):
        self.rates[(original, to)] = rate

    def rate(self, original, to):
        if original == to:
            return 1
        return self.rates[(original, to)]
