#!/usr/bin/env python
# -*- coding:utf-8 -*-

import random
from copy import deepcopy
from collections import deque
import heapq
import math

dir_table = [ ( -1, 0 ), ( 0, 1 ), ( 1, 0 ), ( 0, -1 ), ( 0, 0 ) ]

def make_maze():
    maze_size = 21

    is_wall = lambda x,y: i == 0 or j == 0 or i == maze_size - 1 or j == maze_size - 1 or  ( i % 2 == 0 and j % 2 == 0 )
    maze = [ [ ( is_wall( i, j ) and '#' ) or '.' for i in range( maze_size ) ] for j in range( maze_size ) ]

    for y in range( 2, maze_size - 1, 2 ):
        for x in range( 2, maze_size - 1, 2 ):
            #In the top row, consider the updirection
            dir_seq_range = ( y == 2 and 4  ) or 3
            dir_seq = range( dir_seq_range )
            random.shuffle( dir_seq )
            for d in dir_seq:
                dx,dy = dir_table[ d ]
                next_x, next_y = ( x + dx , y + dy )
                if maze[ next_y ][ next_x ] == '.':
                    maze[ next_y ][ next_x ] = '#'
                    break

    maze[ 1 ][ 1 ] = 'S'
    maze[ maze_size - 2 ][ maze_size - 2 ] = 'G'
    return maze

def set_solved_route( maze, work_maze ):
    result_maze = deepcopy( maze )
    y = len( maze ) - 2
    x = len( maze[ 0 ] ) - 2
    while True:
        di = work_maze [ y ][ x ]
        result_maze[ y ][ x ] = '*'
        if di == 4:
            break;

        dx, dy = dir_table[ di ]
        x -= dx
        y -= dy
    return result_maze

def solve_maze_bfs( maze ):
    work_maze = deepcopy( maze )

    node_queue = deque( [] )
    node_queue.append( ( 1, 1, 4 ) )
    while( node_queue ):
        x, y, di = node_queue.popleft()

        if work_maze[ y ][ x ] == 'G':
            work_maze[ y ][ x ] = di
            break

        if work_maze[ y ][ x ] in range( 5 ):
            continue
        work_maze[ y ][ x ] = di

        for i in range( 4 ):
            dx, dy = dir_table[ i ]
            next_x, next_y = ( x + dx, y + dy )
            next_tile = work_maze[ next_y ][ next_x ]
            if next_tile != '#' and next_tile not in range( 5 ):
                node_queue.append( ( next_x, next_y, i ) )

    result_maze = set_solved_route( maze, work_maze )
    return result_maze

def solve_maze_dfs( maze ):
    work_maze = deepcopy( maze )

    node_stack = []
    node_stack.append( ( 1, 1, 4 ) )
    while( node_stack ):
        x, y, di = node_stack.pop()

        if work_maze[ y ][ x ] == 'G':
            work_maze[ y ][ x ] = di
            break

        if work_maze[ y ][ x ] in range( 5 ):
            continue
        work_maze[ y ][ x ] = di

        for i in range( 4 ):
            dx, dy = dir_table[ i ]
            next_x, next_y = ( x + dx, y + dy )
            next_tile = work_maze[ next_y ][ next_x ]
            if next_tile != '#' and next_tile not in range( 5 ):
                node_stack.append( ( next_x, next_y, i ) )

    result_maze = set_solved_route( maze, work_maze )
    return result_maze

def calc_distance( maze, x, y ):
    return abs( len( maze ) - y ) + abs( len( maze[0] ) - x )

def solve_maze_astar( maze ):
    work_maze = deepcopy( maze )

    node_heap = []
    heapq.heapify( node_heap )
    heapq.heappush( node_heap, ( calc_distance( maze, 1, 1 ), ( 1, 1, 4 ) ) )
    while( node_heap ):
        x, y, di = heapq.heappop( node_heap )[1]

        if work_maze[ y ][ x ] == 'G':
            work_maze[ y ][ x ] = di
            break

        if work_maze[ y ][ x ] in range( 5 ):
            continue
        work_maze[ y ][ x ] = di

        for i in range( 4 ):
            dx, dy = dir_table[ i ]
            next_x, next_y = ( x + dx, y + dy )
            next_tile = work_maze[ next_y ][ next_x ]
            if next_tile != '#' and next_tile not in range( 5 ):
                heapq.heappush( node_heap, ( calc_distance( maze, next_x, next_y ), ( next_x, next_y, i ) ) )

    result_maze = set_solved_route( maze, work_maze )
    return result_maze


def print_maze( maze ):
    for l in maze:
        print( ''.join( map( lambda x : str( x ), l ) ) )
    pass

def main():
    maze = make_maze()
    print( "dfs result" )
    print_maze( solve_maze_dfs( maze ) )
    print( "bfs result" )
    print_maze( solve_maze_bfs( maze ) )
    print( "astar result" )
    print_maze( solve_maze_astar( maze ) )
    return

if __name__ == '__main__':
    main()
