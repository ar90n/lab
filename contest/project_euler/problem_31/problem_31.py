#!/usr/bin/env python

if __name__=='__main__':
    dp = [0] * 201;
    coins = [1, 2, 5, 10, 20, 50, 100, 200]

    dp[0] = 1
    for coin in coins:
        for i in range( 1, 201 ):
            if( 0 <= ( i - coin ) ):
                dp[i] += dp[ i - coin ]
        #print dp
    print dp[200]







