import os, sys, re, math

class TournamentsAmbiguityNumber:
    # int scrutinizeTable(String[] table)
    def scrutinizeTable(self, table):
        n = 0
        for p1_pos, p1 in enumerate( table ):
            for p1_ind in xrange( 0, len( p1 ) ):
                if p1[ p1_ind ] == '1':
                    p2 = table[ int( p1_ind ) ]
                    for p2_ind in xrange( 0, len( p2 ) ):
                        if p2[ p2_ind ] == '1':
                            p3 = table[ int( p2_ind ) ]
                            for p3_ind in xrange( 0, len( p3 ) ):
                                if p3[ p3_ind ] == '1':
                                    if p3_ind == p1_pos :
                                        n += 1
        return n
        pass



# BEGIN CUT HERE

def eq(n, have, need):
    if isinstance(have, list) and isinstance(need, list):
        if len(have) != len(need):
            sys.stdout.write("Test Case #%d...FAILED: returned %d elements; expected %d elements." % (n, len(have), len(need)))

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
    eq(0, TournamentsAmbiguityNumber().scrutinizeTable(["-10", "0-1", "10-"]), 3)
    eq(1, TournamentsAmbiguityNumber().scrutinizeTable(["----", "----", "----", "----"]), 0)
    eq(2, TournamentsAmbiguityNumber().scrutinizeTable(["-1", "0-"]), 0)
    eq(3, TournamentsAmbiguityNumber().scrutinizeTable(["--1-10-1---1--1-00", "--0110000--0---10-", "01--00000100-00011", "-0---0010-11110100", "001--01-00-0001-1-", "11111--100--1-1-01", "-1110--00110-11-01", "0110-01--100110-10", "-111111---01--0-01", "--0-1100----10011-", "--10--011--1--101-", "01101-110-0--1-0-1", "---010-0-0---00-11", "--101-00-1-01-0-0-", "0-110001110-11-110", "-010-----011--0--0", "11010110100-010--0", "1-01-0010--00-111-"]), 198)

# END CUT HERE
