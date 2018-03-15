#! /usr/bin/env python3
# -*- coding: utf-8 -*-

class TestCase:

    def __init__(self, name):
        self.name = name

    def run(self):
        method = getattr(self, self.name)
        method()


class WasRun(TestCase):

    def __init__(self, name):
        self.wasRun = None
        super().__init__(name)

    def testMethod(self):
        self.wasRun = 1


class TestCaseTest(TestCase):

    def testRunning(self):
        test = WasRun('testMethod')
        assert not test.wasRun
        test.run()
        assert test.wasRun


def main():
    TestCaseTest('testRunning').run()

if __name__ == '__main__':
    main()
