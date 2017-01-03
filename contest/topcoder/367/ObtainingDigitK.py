import os, sys, re, math

class ObtainingDigitK:
    # int minNumberToAdd(String originalNumber, int k)
    def minNumberToAdd(self, originalNumber, k):
        return [ str( int(originalNumber) + x).find( str(k) ) == -1 for x in xrange( 0, 10 ) ].index( False )
        pass



# BEGIN CUT HERE

def eq(n, have, need):
    if isinstance(have, list) and isinstance(need, list):
        if len(have) != len(need):
            sys.stdout.write("Test Case #%d...FAILED: returned %d elements; expected %d elements." % (n, len(have), len(need)))
            printerror(have, need)
        else:
            for i in range(0, len(have)):
                if not eqval(have[i], need[i]):
                    sys.stdout.write("Test Case #%d...FAILED: Expected and returned array differ in position %d.\n" % (n, i))
                    printerror(have, need)
                    return
            sys.stdout.write("Test Case #%d...PASSED\n" % n)
    else:
        if eqval(have, need):
            sys.stdout.write("Test Case #%d...PASSED\n" % n)
        else:
            sys.stdout.write("Test Case #%d...FAILED\n" % n)
            printerror(have, need)

def printerror(have, need):
    sys.stdout.write("\tExpected: " + printval(need) + "\n")
    sys.stdout.write("\tRecieved: " + printval(have) + "\n")

def printval(a):
    if isinstance(a, str):
        return '"' + a + '"'
    elif isinstance(a, long):
        return str(a) + 'L'
    else:
        return str(a)

def eqval(a, b):
    if isinstance(a, float) or isinstance(b, float):
        return (abs(a - b) < 1e-9 or (abs(a) >= 1 and abs((a - b) / a) < 1e-9))
    else:
        return (a != None and b != None and a == b)

if __name__ == "__main__":
    eq(0, ObtainingDigitK().minNumberToAdd("153", 7), 4)
    eq(1, ObtainingDigitK().minNumberToAdd("158", 7), 9)
    eq(2, ObtainingDigitK().minNumberToAdd("7853192", 2), 0)
    eq(3, ObtainingDigitK().minNumberToAdd("99999999999999999999999999999999999999999999999", 0), 1)

# END CUT HERE
