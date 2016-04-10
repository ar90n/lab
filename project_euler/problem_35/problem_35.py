#!/usr/bin/env python
import math
import random

def isPrime( q, k=50  ):
    q = abs( q )

    if q == 2:
        return True
    if q < 2 or q & 1 == 0 :
        return False

    d = ( q - 1 ) >> 1
    while d & 1 == 0:
        d >>= 1

    ass = [ x for x in [ 2, 7, 61] if x < q ]
    for a in ass:
        #a = random.randint( 1, q -1 )
        t = d
        y = pow( a, t, q )
        while t != q - 1 and y != 1 and y != q - 1:
            y = pow( y, 2, q )
            t <<= 1
        if y != q - 1 and t & 1 == 0 :
            return False
    return True

def isEven( n ):
    return map(lambda x: ( int(x) & 1 ) == 0  , list( str( n ) ) ).count( True ) ==  0

if __name__=='__main__':

    primes = [2]
    for n in xrange(3, 1000000,  2):
        if isEven( n ):
            if isPrime( n ):
                primes.append( n )

    count = 0
    for n in primes:
        list_n = list( str(n) )

        try:
            for i in range( len( list_n ) ) :
                if( not ( int( ''.join( list_n )  ) in primes ) ):
                    raise
                list_n.append( list_n.pop( 0 ) )
            count += 1
        except:
            continue

    print count
