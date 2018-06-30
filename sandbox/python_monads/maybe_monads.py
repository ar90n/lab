#! /usr/bin/env python3
# -*- coding: utf-8 -*-


def of(value):
    return Nothing() if value is None else Just(value)


class Just:

    def __init__(self, value):
        self.value = value

    def map(self, f):
        return of(f(self.value))

    def get(self, value):
        return self.value

    def filter(self, f):
        return of(self.value if f(self.value) else None)

    def chain(self, f):
        return f(self.value)

    def is_just(self):
        return True

    def is_nothing(self):
        return False


class Nothing:

    def __init__(self):
        pass

    def map(self, f):
        return self

    def get(self, value=None):
        if value is None:
            raise ValueError('Can\'t extract the value of a Nothing.')
        return value

    def filter(self, f):
        return self

    def chain(self, f):
        return self


def main():
    v = of(100).map(lambda x: x * x).filter(lambda x: x % 3 == 0).get(0)
    print(v)


if __name__ == '__main__':
    main()
