#!/usr/bin/env python
# -*- coding:utf-8 -*-

import collections
import sys

def add_node( g, src, dst, w ):
    g[ src ][ dst ] = w
    pass

def find_bellman_ford( g, src ):
    all_nodes = reduce( lambda x,y: x.union( y ), [ set( g.keys() ) ] + [ set( g[ x ].keys() ) for x in g ] )
    node_num = len( all_nodes )
    min_cost = dict( zip( all_nodes, [ sys.maxint ] * node_num ) )

    min_cost[ src ] = 0
    for i in range( node_num ):
        is_changed = False
        for node,neighbors in zip( g.keys(), g.values() ):
            for neighbor,w in zip( neighbors.keys(), neighbors.values() ):
                new_cost = min_cost[ node ] + w
                current_cost = min_cost[ neighbor ]
                if new_cost < current_cost:
                    min_cost[ neighbor ] = new_cost
                    is_changed = True
        if not is_changed:
            break
    return min_cost
    pass

def main():
    g = collections.defaultdict( dict )
    add_node( g, 0, 1, 1 )
    add_node( g, 1, 3, 1 )
    add_node( g, 3, 4, 1 )
    add_node( g, 0, 2, 4 )
    add_node( g, 2, 4, 5 )
    add_node( g, 4, 2, -1 )
    res = find_bellman_ford( g, 0 )
    print( res )
    return

if __name__ == '__main__':
    main()
