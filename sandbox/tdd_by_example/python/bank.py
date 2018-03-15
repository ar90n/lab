#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .money import Money


class Bank:

    def __init__(self):
        pass

    def reduce(self, source, to):
        return source.reduce(to)
