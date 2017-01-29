#!/usr/bin/env python
# -*- coding:utf-8 -*-

from functools import reduce
from collections import defaultdict 
import sys

def find_warshall_floyd( g ):
    all_nodes = list( reduce( lambda x, y: x.union( y ), [ set( g.keys() ) ] + [ set( x.keys() ) for x in g.values() ] ) )
    node_num = len( all_nodes )

    dist = [ [ sys.maxsize for i in range( node_num ) ] for j in range( node_num ) ]
    for node in all_nodes:
        dist[ node ][ node ] = 0
    for node,neighbors in zip( g.keys(), g.values() ):
        for neighbor,w in zip( neighbors.keys(), neighbors.values() ):
            dist[ node ][ neighbor ] = w

    for k in all_nodes:
        for i in all_nodes:
            for j in all_nodes:
                dist[ i ][ j ] = min( dist[ i ][ j ], dist[ i ][ k ] + dist[ k ][ j ] ) 

    return dist

def main():
    g = defaultdict( dict )
    g[ 0 ][ 1 ] = 1
    g[ 1 ][ 3 ] = 1
    g[ 3 ][ 4 ] = 1
    g[ 0 ][ 2 ] = 4
    g[ 2 ][ 4 ] = 5
    res = find_warshall_floyd( g )
    for l in res:
        print( l )
    return

if __name__ == '__main__':
    main()
