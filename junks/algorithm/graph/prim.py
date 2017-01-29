#!/usr/bin/env python
# -*- coding:utf-8 -*-

import collections
import heapq
import sys

def find_prim( g, src ):
    min_spaning_tree = collections.defaultdict( dict )

    edge_heap = []
    heapq.heapify( edge_heap )
    heapq.heappush( edge_heap, ( 0, ( src, src ) ) )
    while edge_heap:
        cost, edge = heapq.heappop( edge_heap )
        if edge[1] in min_spaning_tree:
            continue
        min_spaning_tree[ edge[ 0 ] ][ edge[ 1 ] ] = cost
        min_spaning_tree[ edge[ 1 ] ][ edge[ 0 ] ] = cost

        for neighbor,w in zip( g[ edge[1] ].keys(), g[ edge[1] ].values() ):
            heapq.heappush( edge_heap, ( w, ( edge[ 1 ], neighbor ) ) )

    min_spaning_tree[ src ].pop( src )
    return min_spaning_tree
    pass

def main():
    g = collections.defaultdict( dict )
    g[ 0 ][ 1 ] = g[ 1 ][ 0 ] = 1
    g[ 1 ][ 2 ] = g[ 2 ][ 1 ] = 4
    g[ 0 ][ 2 ] = g[ 2 ][ 0 ] = 1
    g[ 0 ][ 3 ] = g[ 3 ][ 0 ] = 1
    g[ 2 ][ 3 ] = g[ 3 ][ 2 ] = 1
    g[ 0 ][ 4 ] = g[ 4 ][ 0 ] = 7
    g[ 3 ][ 4 ] = g[ 4 ][ 3 ] = 2
    g[ 0 ][ 5 ] = g[ 5 ][ 0 ] = 4
    g[ 4 ][ 5 ] = g[ 5 ][ 4 ] = 3

    res = find_prim( g, 0 )
    print( g )
    print( res )
    return

if __name__ == '__main__':
    main()
