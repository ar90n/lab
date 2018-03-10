#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .dollar import Dollar
from .franc import Franc


def test_multiplication():
    five = Dollar(5)
    assert five.times(2) == Dollar(10)
    assert five.times(3) == Dollar(15)


def test_equality():
    assert Dollar(5) == Dollar(5)
    assert Dollar(5) != Dollar(6)
    assert Franc(5) == Franc(5)
    assert Franc(5) != Franc(6)
    assert Franc(5) != Dollar(5)


def test_franc_multiplication():
    five = Franc(5)
    assert five.times(2) == Franc(10)
    assert five.times(3) == Franc(15)
