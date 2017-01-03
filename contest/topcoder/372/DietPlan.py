import os, sys, re, math
import string

class DietPlan:
    # String chooseDinner(String diet, String breakfast, String lunch)
    def chooseDinner(self, diet, breakfast, lunch):
        diet = ''.join( sorted( list( diet ) ) )

        diet1 = diet.translate( string.maketrans( "", "" ), breakfast )
        if len( diet1 ) != ( len( diet ) - len( breakfast )):
            return 'CHEATER'

        diet2 = diet1.translate( string.maketrans( "", "" ), lunch )
        if len( diet2 ) != ( len( diet1 ) - len( lunch ) ):
            return 'CHEATER'

        return diet2
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
    eq(0, DietPlan().chooseDinner("ABCD", "AB", "C"), "D")
    eq(1, DietPlan().chooseDinner("ABEDCS", "", ""), "ABCDES")
    eq(2, DietPlan().chooseDinner("EDSMB", "MSD", "A"), "CHEATER")
    eq(3, DietPlan().chooseDinner("", "", ""), "")
    eq(4, DietPlan().chooseDinner("IWANTSODER", "SOW", "RAT"), "DEIN")

# END CUT HERE
