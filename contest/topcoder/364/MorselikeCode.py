import os, sys, re, math

class MorselikeCode:
    # String decrypt(String[] library, String message)
    def decrypt(self, library, message):
        dic_lib = dict([ reversed(x.split(' ' )) for x in library ])
        return ''.join( [ dic_lib.has_key( x ) and dic_lib[ x ] or '?' for x in message.split(' ') ] )
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
    eq(0, MorselikeCode().decrypt(["O ---", "S ..."], "... --- ..."), "SOS")
    eq(1, MorselikeCode().decrypt(["O ---"], "... --- ..."), "?O?")
    eq(2, MorselikeCode().decrypt(["H -", "E .", "L --", "L ..", "O -."], "- . -- .. -."), "HELLO")
    eq(3, MorselikeCode().decrypt(["H -.-.-.-.-.-.-.-.-.-.", "I .-.-.-.-.-.-.-.-.-.-", "K -.-.-.-.-.", "E .-.-.-.-.-"], "-.-.-.-.-.-.-.-.-.-. .-.-.-.-.-.-.-.-.-.-"), "HI")
    eq(4, MorselikeCode().decrypt(["O ---", "S ...", "B -...", "T -", "R .-.", "E .", "N -.", "X -..-", "D -.."], "-... --- ... - --- -. .-. . -.. ... --- -..-"), "BOSTONREDSOX")
    eq(5, MorselikeCode().decrypt(["B -...", "N -.", "H ....", "O --", "Z --..", "G ---", "I ..", "J .---"], "--- -- -... .- -. .- -. .-"), "GOB?N?N?")
    eq(6, MorselikeCode().decrypt(["A --", "B -.", "N ...-", "I --..", "F -.-.-."], "-. -- -.-.-. -.-.-. --- --.. ...- .-..--."), "BAFF?IN?")

# END CUT HERE
