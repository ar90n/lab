#!/usr/bin/env python

if __name__=='__main__':
    dp = [ 1, 1, 2 ]

    res = 0
    for num in range( 3, 100000 ):
        sum = 0

        for n in map( int , list( str( num ) ) ):
            if( len( dp ) - 1 < n ):
                for i in range( len( dp ), n + 1 ):
                    dp.append( dp[-1] * i )

            sum += dp[n]
        if( sum == num ):
            print num
            res += num

    print res


