import os, sys, re, math

class Sets:
    # int[] operate(int[] A, int[] B, String operation)
    def operate(self, A, B, operation):
	if operation == "UNION":
		return sorted( list( set( A ).union( set( B ) ) ) )
	elif operation == "INTERSECTION":
		return sorted( list( set(A).intersection( set(B) ) ) )
	elif operation == "SYMMETRIC DIFFERENCE":
		return sorted( list( set(A).symmetric_difference( set(B) ) ) )
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
    eq(0, Sets().operate([1,2,3,4], [3,4,5,6], "INTERSECTION"), [ 3,  4 ])
    eq(1, Sets().operate([1,2,3,4], [3,4,5,6], "UNION"), [ 1,  2,  3,  4,  5,  6 ])
    eq(2, Sets().operate([432,756,123], [534,76,1209], "INTERSECTION"), [ ])
    eq(3, Sets().operate([6,5,7,4], [7,6,4,10], "SYMMETRIC DIFFERENCE"), [ 5,  10 ])
    eq(4, Sets().operate([342,654,897,312,76,23,78], [21,43,87,98,23,756,897,234,645,876,123], "SYMMETRIC DIFFERENCE"), [ 21,  43,  76,  78,  87,  98,  123,  234,  312,  342,  645,  654,  756,  876 ])

# END CUT HERE
