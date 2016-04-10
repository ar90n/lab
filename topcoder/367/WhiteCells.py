import os, sys, re, math

class WhiteCells:
    # int countOccupied(String[] board)
    def countOccupied(self, board):
        return ''.join( [ x[1][(x[0]%2)::2] for x in enumerate( board ) ] ).count('F')
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
    eq(0, WhiteCells().countOccupied(["........", "........", "........", "........", "........", "........", "........", "........"]), 0)
    eq(1, WhiteCells().countOccupied(["FFFFFFFF", "FFFFFFFF", "FFFFFFFF", "FFFFFFFF", "FFFFFFFF", "FFFFFFFF", "FFFFFFFF", "FFFFFFFF"]), 32)
    eq(2, WhiteCells().countOccupied([".F.F...F", "F...F.F.", "...F.F.F", "F.F...F.", ".F...F..", "F...F.F.", ".F.F.F.F", "..FF..F."]), 1)
    eq(3, WhiteCells().countOccupied(["........", "..F.....", ".....F..", ".....F..", "........", "........", ".......F", ".F......"]), 2)

# END CUT HERE
