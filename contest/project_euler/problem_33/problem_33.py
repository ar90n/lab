#!/usr/bin/env python

import fractions

if __name__=='__main__':

    res_num = 1
    res_den = 1
    for den in range( 10, 100 ):
        for num in range( 10, den ):
            ln = list( str( num ) )
            ld = list( str( den ) )
            sn = set( ln )
            sd = set( ld )
            for i in sn.intersection( sd ):
                if i == '0':
                    continue;

                if ln[0] == i:
                    nn = int( ln[1] )
                else:
                    nn = int( ln[0] )

                if ld[0] == i:
                    nd = int( ld[1] )
                else:
                    nd = int( ld[0] )

                if nd == 0:
                    break;

                if num * nd  == nn * den:
                    res_num *= num
                    res_den *= den
                    com = fractions.gcd( res_num, res_den )
                    res_num //= com
                    res_den //= com
                pass

    print( res_den )

