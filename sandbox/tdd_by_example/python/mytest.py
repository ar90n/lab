#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .money import Money
from .bank import Bank
from .sum import Sum


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


def test_plug_returns_sum():
    five = Money.dollar(5)
    sum = five + five
    assert sum.augend == five
    assert sum.addend == five


def test_reduce_sum():
    sum = Sum(Money.dollar(3), Money.dollar(4))
    bank = Bank()
    result = bank.reduce(sum, 'USD')
    assert result == Money.dollar(7)


def test_reduce_money():
    bank = Bank()
    result = bank.reduce(Money.dollar(1), 'USD')
    assert result == Money.dollar(1)


def test_reduce_money_different_currency():
    bank = Bank()
    bank.addRate('CHF', 'USD', 2)
    result = bank.reduce(Money.franc(2), 'USD')
    assert result == Money.dollar(1)


def test_identity_rate():
    assert Bank().rate('USD', 'USD') == 1
