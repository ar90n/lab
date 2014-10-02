#!/usr/bin/env python

if __name__=='__main__':
    flags = [[0] * 101] * 101
    prime = [ p for p in range( 2, 100 ) if 0 not in [ p %d for d in range(2, p) ] ]

    num = [1] + prime

    for i in range(1 , len(num)):
        denom = num[i]
        #print 'denom:', denom
        for nume in  num[:i]:
            count = 100 / denom
        #    print count
            for j in range( count ):
                flags[ nume * j ][ denom * j ] = 1

    for i in range(1, 101):
        for j in range(2, 101):
            if( flags[i][j] == 0 ):
                if( j % i == 0 ):
                    print i, j
                    pass
