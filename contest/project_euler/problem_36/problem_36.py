#!/usr/bin/env python

def isPali2( n ):
    bin = ""
    while( n > 0 ):
        bin += ( ( n & 1 ) and '1' ) or '0'
        n >>= 1
    return bin == bin[::-1]

def isPali10( n ):
    return n == int( ''.join( list( str( n ) )[::-1] ) )

if __name__=='__main__':

    palis = []
    for i in xrange( 1000000 ):
        if isPali10( i ) and isPali2( i ):
            palis.append( i )

    print palis
    print sum(palis)
