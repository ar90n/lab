#!/usr/bin/env python

def fpnum( num ):
    val = 0
    while( num > 0 ):
        current = num % 10
        val += current * current * current * current * current
        num /= 10

    return val

if __name__=='__main__':
    sum_val = 0
    for n in range( 2, 300000 ):
        if( n == fpnum( n ) ):
            print n
            sum_val += n

    print sum_val
