#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .money import Money
from .bank import Bank


def test_multiplication():
    five = Money.dollar(5)
    assert five.times(2) == Money.dollar(10)
    assert five.times(3) == Money.dollar(15)


def test_equality():
    assert Money.dollar(5) == Money.dollar(5)
    assert Money.dollar(5) != Money.dollar(6)
    assert Money.franc(5) != Money.dollar(5)


def test_franc_multiplication():
    five = Money.franc(5)
    assert five.times(2) == Money.franc(10)
    assert five.times(3) == Money.franc(15)


def test_currency():
    assert 'USD' == Money.dollar(1).currency
    assert 'CHF' == Money.franc(1).currency


def test_simple_addition():
    sum = Money.dollar(5) + Money.dollar(5)
    bank = Bank()
    reduced = bank.reduce(sum, 'USD')
    assert reduced == Money.dollar(10)
