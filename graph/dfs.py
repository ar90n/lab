#!/usr/bin/env python
# -*- coding:utf-8 -*-

class Vertex(object):
    pass

class Edge( object ):
    def __init__( self, begin, end ):
        self.begin = begin
        self.end = end
        pass

class Graph(object):
    def __init__( self, vertice, edges ):
        self.vertice = vertice
        self.edges = edges
        pass

    def get_edges( self, v ):
        return self.edges[ v ]

visited = set([])
def dfs( graph, vertex, target ):
    visited.clear()
    return dfs_impl2( graph, vertex, target )

def dfs_impl1( graph, vertex, target ):
    if vertex == target:
        return True

    if vertex in visited:
        return False
    visited.add( vertex )

    for next_vertex in graph.get_edges( vertex ):
        if dfs( graph, next_vertex, target):
            return True

    return False

def dfs_impl2( graph, vertex, target ):
    vertex_stack = [ vertex ]

    while( vertex_stack ):
        current_vertex = vertex_stack.pop()
        if current_vertex == target:
            return True

        for next_vertex in graph.get_edges( current_vertex ):
            vertex_stack.append( next_vertex )

    return False

def main():
    vertice = [ Vertex() for i in range( 7 ) ];
    edges = {}
    edges[ vertice[ 0 ] ] = [ vertice[ i ] for i in [ 1, 4, 6 ] ] 
    edges[ vertice[ 1 ] ] = [ vertice[ i ] for i in [ 2, 3 ] ] 
    edges[ vertice[ 2 ] ] = [] 
    edges[ vertice[ 3 ] ] = [] 
    edges[ vertice[ 4 ] ] = [ vertice[ i ] for i in [ 3, 5 ] ] 
    edges[ vertice[ 5 ] ] = [ vertice[ i ] for i in [ 3 ] ] 
    edges[ vertice[ 6 ] ] = [] 

    g = Graph( vertice, edges )
    print( dfs( g, vertice[ 0 ], vertice[ 3 ] ) )
    print( dfs( g, vertice[ 0 ], Vertex() ) )


    return

if __name__ == '__main__':
    main()
