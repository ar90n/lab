#!/usr/bin/env python
# -*- coding:utf-8 -*-

from collections import deque

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
def toposort( graph, vertex, target ):
    visited.clear()
    return bfs_impl( graph, vertex, target )

def bfs_impl( graph, vertex, target ):
    vertex_queue = deque( [ vertex ] )

    while( vertex_queue ):
        current_vertex = vertex_queue.popleft()
        if current_vertex == target:
            return True

        if current_vertex in visited:
            continue
        visited.add( current_vertex )

        for next_vertex in graph.get_edges( current_vertex ):
            vertex_queue.append( next_vertex )

    return False

def main():
    vertice = [ Vertex() for i in range( 14 ) ];
    edges = {}
    edges[ vertice[ 0 ] ] = [ vertice[ i ] for i in [ 2 ] ] 
    edges[ vertice[ 1 ] ] = [ vertice[ i ] for i in [ 4 ] ] 
    edges[ vertice[ 2 ] ] = [ vertice[ i ] for i in [ 4, 5 ] ] 
    edges[ vertice[ 3 ] ] = [ vertice[ i ] for i in [ 6 ] ] 
    edges[ vertice[ 4 ] ] = [ vertice[ i ] for i in [ 7 ] ] 
    edges[ vertice[ 5 ] ] = [ vertice[ i ] for i in [ 7 ] ] 
    edges[ vertice[ 6 ] ] = [ vertice[ i ] for i in [ 8 ] ] 
    edges[ vertice[ 7 ] ] = [ vertice[ i ] for i in [ 8, 9 ] ] 
    edges[ vertice[ 8 ] ] = [ vertice[ i ] for i in [ 10 ] ] 
    edges[ vertice[ 9 ] ] = [ vertice[ i ] for i in [ 11 ] ] 
    edges[ vertice[ 10 ] ] = [ vertice[ i ] for i in [ 12 ] ] 
    edges[ vertice[ 11 ] ] = [ vertice[ i ] for i in [ 12 ] ] 
    edges[ vertice[ 12 ] ] = [ vertice[ i ] for i in [ 13 ] ] 
    edges[ vertice[ 14 ] ] = []

    g = Graph( vertice, edges )
    print( toposort( g )
    return

if __name__ == '__main__':
    main()
