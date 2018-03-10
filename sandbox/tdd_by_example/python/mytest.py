#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .dollar import Dollar


def test_multiplication():
    five = Dollar(5)
    product = five.times(2)
    assert product.amount == 10
    product = five.times(3)
    assert product.amount == 15


def test_equality():
    assert Dollar(5) == Dollar(5)
    assert Dollar(5) != Dollar(6)
