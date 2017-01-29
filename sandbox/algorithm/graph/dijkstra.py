#!/usr/bin/env python
# -*- coding:utf-8 -*-

import collections
import heapq
import sys

def find_dijkstra( g, src ):
    min_cost = {}

    node_heap = []
    heapq.heapify( node_heap )
    heapq.heappush( node_heap, ( 0, src ) )
    while node_heap:
        cost,node = heapq.heappop( node_heap )
        if node in min_cost:
            continue
        min_cost[ node ] = cost

        for neighbor,w in zip( g[ node ].keys(), g[ node ].values() ):
            neighbor_cost = min_cost[ node ] + w
            heapq.heappush( node_heap, ( neighbor_cost, neighbor ) )
    return min_cost
    pass

def main():
    g = collections.defaultdict( dict )
    g[ 0 ][ 1 ] = 1
    g[ 1 ][ 3 ] = 1
    g[ 3 ][ 4 ] = 1
    g[ 0 ][ 2 ] = 4
    g[ 2 ][ 4 ] = 5
    res = find_dijkstra( g, 0 )
    return

if __name__ == '__main__':
    main()
