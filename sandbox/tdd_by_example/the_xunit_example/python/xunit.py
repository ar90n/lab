#! /usr/bin/env python3
# -*- coding: utf-8 -*-

class TestResult:

    def __init__(self):
        self.runCount = 0

    def testStarted(self):
        self.runCount += 1

    def summary(self):
        return '%d run, 0 failed' % self.runCount


class TestCase:

    def __init__(self, name):
        self.name = name

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def run(self):
        result = TestResult()
        result.testStarted()
        self.setUp()
        method = getattr(self, self.name)
        method()
        self.tearDown()
        return result


class WasRun(TestCase):

    def setUp(self):
        self.wasRun = None
        self.log = 'setUp '

    def testMethod(self):
        self.wasRun = 1
        self.log += 'testMethod '

    def testBrokenMethod(self):
        raise Exception()

    def tearDown(self):
        self.log += 'tearDown '


class TestCaseTest(TestCase):

    def testTemplateMethod(self):
        test = WasRun('testMethod')
        test.run()
        assert test.log == 'setUp testMethod tearDown '

    def testResult(self):
        test = WasRun('testMethod')
        result = test.run()
        assert result.summary() == '1 run, 0 failed'

    def testFailedResult(self):
        test = WasRun('testBrokenMethod')
        result = test.run()
        assert result.summary() == '1run, 1 failed'


def main():
    TestCaseTest('testTemplateMethod').run()
    TestCaseTest('testResult').run()
    # TestCaseTest('testFailedResult').run()

if __name__ == '__main__':
    main()
