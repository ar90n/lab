#!/usr/bin/env python
# -*- coding:utf-8 -*-

from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import
#from future_builtins import *

import sys
import networkx as nx
from collections import deque

def find_source_to_sink_path( rg, source, sink ):
    bfs_queue = deque( [ source ])
    bfs_visited = set([])
    parents_map = { source : -1 }
    source_to_sink_path = []

    flow = sys.float_info.max
    while bfs_queue:
        current_node = bfs_queue.popleft()
        bfs_visited.add( current_node )

        if current_node == sink:
            r_current_node = current_node
            r_parent_node = parents_map[ r_current_node ]
            while r_parent_node != -1:
                capacity = rg[r_parent_node][r_current_node]['capacity']
                flow = min( flow, capacity )
                source_to_sink_path.append( r_current_node )
                r_current_node = r_parent_node
                r_parent_node = parents_map[ r_parent_node ]
            source_to_sink_path.append( source )
            source_to_sink_path.reverse()
            return source_to_sink_path, flow, set([])

        for next_node in rg[ current_node ].keys():
            if 0 < rg[ current_node ][ next_node ][ 'capacity'] and next_node not in bfs_visited:
                parents_map[ next_node ] = current_node
                bfs_queue.append( next_node )

    return [], 0.0, set(bfs_visited)

def find_maxflow_ek( rg, source, sink ):
    maxflow = 0.0
    while True:
        source_to_sink_path, current_flow, source_nodes = find_source_to_sink_path( rg, source, sink )
        print( source_to_sink_path, source_nodes )
        if current_flow == 0.0:
            break;

        maxflow += current_flow
        for from_node, to_node in zip( source_to_sink_path[0:-1], source_to_sink_path[1:]):
            rg[ from_node ][ to_node ][ 'capacity'] -= current_flow
            rg[ to_node ][ from_node ][ 'capacity'] += current_flow

    sink_nodes = set( rg.nodes() ) - source_nodes
    return source_nodes, sink_nodes, maxflow


def main( ):
    g = nx.DiGraph()

    g.add_node( 0 )
    g.add_node( 1 )
    g.add_node( 2 )
    g.add_node( 3 )

    g.add_edge( 0, 1, { "capacity" : 8 } )
    g.add_edge( 0, 2, { "capacity" : 2 } )
    g.add_edge( 1, 2, { "capacity" : 1 } )
    g.add_edge( 1, 3, { "capacity" : 3 } )
    g.add_edge( 2, 3, { "capacity" : 7 } )

    rg = g.copy()
    rg.add_edge( 1, 0, { "capacity" : 0 } )
    rg.add_edge( 2, 0, { "capacity" : 0 } )
    rg.add_edge( 2, 1, { "capacity" : 0 } )
    rg.add_edge( 3, 1, { "capacity" : 0 } )
    rg.add_edge( 3, 2, { "capacity" : 0 } )

    source_nodes, sink_nodes, maxflow = find_maxflow_ek( rg, 0, 3 )

    print( source_nodes, sink_nodes, maxflow )

    return


if __name__ == '__main__':
    main( )
