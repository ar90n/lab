import os, sys, re, math

class StreetParking:
    # int freeParks(String street)
    def freeParks(self, street):
	ok = 0
	for i in range( len( street ) ):
		if 'B' in street[ i : min( i + 3, len( street ) ) ]:
			continue
		if 'D' == street[i]:
			continue
		if 'S' in street[ max( i - 1 , 0 ) : min( i + 2, len( street ) ) ]:
			continue
		ok += 1
	return ok



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
    eq(0, StreetParking().freeParks("---B--S-D--S--"), 4)
    eq(1, StreetParking().freeParks("DDBDDBDDBDD"), 0)
    eq(2, StreetParking().freeParks("--S--S--S--S--"), 2)
    eq(3, StreetParking().freeParks("SSD-B---BD-DDSB-----S-S--------S-B----BSB-S--B-S-D"), 14)

# END CUT HERE
