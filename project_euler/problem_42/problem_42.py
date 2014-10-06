#!/usr/bin/env python
# -*- coding:utf-8 -*-

def main():
    words = eval( "[" + open( 'p042_words.txt' ).readline() + "]" )
    max_tr_num = 26 * max( map( lambda x : len( x ), words ) )

    i = 1
    tr_num = {}
    while True:
        n = ( i * ( i + 1 ) ) / 2
        tr_num[ n ] = 1

        if max_tr_num < n:
            break;
        i += 1

    res = 0
    for w in words:
        tn = sum( [ 1 + ord( a ) - ord( 'A' ) for a in list(w) ] )
        if tr_num.has_key( tn ):
            res += 1

    print( res )
    return

if __name__ == '__main__':
    main()
