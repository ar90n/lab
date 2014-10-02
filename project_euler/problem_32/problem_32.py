#!/usr/bin/env python

if __name__=='__main__':
     res = []
     for first in range( 11,  100 ):
        for second in range( 100,  1000 ):
            mul = first * second;
            res_str = str( first ) + str( second ) + str( mul )
            if( ( res_str.find('0') == -1 ) and ( len( set( res_str ) ) == len( res_str ) ) ):
                res.append( mul )
     for first in range( 1,  10 ):
        for second in range( 1000,  10000 ):
            mul = first * second;
            res_str = str( first ) + str( second ) + str( mul )
            if( ( res_str.find('0') == -1 ) and ( len( set( res_str ) ) == len( res_str ) ) ):
                res.append( mul )

     print sum( set( res ) )
