#! /usr/bin/env python3
# -*- coding: utf-8 -*-

class TestCase:

    def __init__(self, name):
        self.name = name

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def run(self):
        self.setUp()
        method = getattr(self, self.name)
        method()
        self.tearDown()


class WasRun(TestCase):

    def setUp(self):
        self.wasRun = None
        self.log = 'setUp '

    def testMethod(self):
        self.wasRun = 1
        self.log += 'testMethod '

    def tearDown(self):
        self.log += 'tearDown '


class TestCaseTest(TestCase):

    def testTemplateMethod(self):
        test = WasRun('testMethod')
        test.run()
        assert test.log == 'setUp testMethod tearDown '


def main():
    TestCaseTest('testTemplateMethod').run()

if __name__ == '__main__':
    main()
