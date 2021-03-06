#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from .money import Money
from .bank import Bank
from .sum import Sum


def test_multiplication():
    five = Money.dollar(5)
    assert five * 2 == Money.dollar(10)
    assert 2 * five == Money.dollar(10)
    assert 3 * five == Money.dollar(15)


def test_equality():
    assert Money.dollar(5) == Money.dollar(5)
    assert Money.dollar(5) != Money.dollar(6)
    assert Money.franc(5) != Money.dollar(5)


def test_franc_multiplication():
    five = Money.franc(5)
    assert 2 * five == Money.franc(10)
    assert 3 * five == Money.franc(15)


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


def test_mixed_addition():
    fiveBucks = Money.dollar(5)
    tenFrancs = Money.franc(10)
    bank = Bank()
    bank.addRate('CHF', 'USD', 2)
    result = bank.reduce(fiveBucks + tenFrancs, 'USD')
    assert result == Money.dollar(10)


def test_sum_plus_money():
    fiveBucks = Money.dollar(5)
    tenFrancs = Money.franc(10)
    bank = Bank()
    bank.addRate('CHF', 'USD', 2)
    sum = Sum(fiveBucks, tenFrancs) + fiveBucks
    result = bank.reduce(sum, 'USD')
    assert result == Money.dollar(15)


def test_sum_times():
    fiveBucks = Money.dollar(5)
    tenFrancs = Money.franc(10)
    bank = Bank()
    bank.addRate('CHF', 'USD', 2)
    sum = Sum(fiveBucks, tenFrancs) * 2
    result = bank.reduce(sum, 'USD')
    assert result == Money.dollar(20)
