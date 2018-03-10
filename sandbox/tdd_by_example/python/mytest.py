#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .dollar import Dollar


def test_multiplication():
    five = Dollar(5)
    five.times(2)
    assert five.amount == 10
