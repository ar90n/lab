#!/usr/bin/env python
# -*- coding:utf-8 -*-

from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import
from future_builtins import *

import itertools
import copy

import cv2
import numpy as np
import networkx as nx

def create_image():
    img = np.zeros( ( 64, 64 ))
    img[16:48, 16:48] = 255
    img[24:40, 24:40] = 0
    noise = np.random.rand(64,64)
    img[ noise < 0.1 ] = 0
    img[ 0.9 < noise ] = 255
    return img

def point_to_index( i, x, y, w, h ):
    return 2 + ( i * w * h + y * w + x )

def index_to_point( index, w, h ):
    i = ( ( index - 2 ) // ( w * h ) )
    y = ( ( index - 2 ) %  ( w * h ) ) // w
    x = ( ( index - 2 ) %  ( w * h ) ) %  w
    return ( i, x, y )

def main( ):
    lamb = 40
    kappa = 9 * 255
    infinity = 100000

    img = create_image()
    w = img.shape[0]
    h = img.shape[1]

    g = nx.DiGraph()
    g.add_nodes_from( range( w * h * 1 + 2 ) )
    for x,y in itertools.product( range( w ), range( h ) ):
        cost = abs( img[y,x] - 0 ) * lamb
        index = point_to_index( 0, x, y, w, h )
        g.add_edge( 0, index, { 'capacity' : cost } )

        cost = abs( img[y,x] - 255 ) * lamb
        index = point_to_index( 0, x, y, w, h )
        g.add_edge( index, 1, { 'capacity' : cost } )

    dxs = [ 1, -1, 0, 1 ]
    dys = [ 0,  1, 1, 1 ]
    for i,x,y in itertools.product( range( 1 ), range( w ), range( h ) ):
        index = point_to_index( i, x, y, w, h )
        for dx,dy in zip( dxs, dys ):
            neighbor_x = x + dx
            neighbor_y = y + dy
            if 0 <= neighbor_x and neighbor_x < w and 0 <= neighbor_y and neighbor_y < h:
                neighbor_index = point_to_index( i, neighbor_x, neighbor_y, w, h )
                g.add_edge( index, neighbor_index, { 'capacity' : kappa } )
                g.add_edge( neighbor_index, index, { 'capacity' : kappa } )

    cut_value, partition = nx.minimum_cut( g, 0, 1 )
    reachable, non_reachable = partition

    cutset = set()
    for u,nbrs in ( (n, g[n]) for n in reachable ):
        cutset.update( (u,v) for v in nbrs if v in non_reachable )

    res = copy.deepcopy( img )
    for ind1,ind2 in cutset:
        if ind1 <= 1 or ind2 <= 1:
            i,x,y = index_to_point( max(ind1,ind2), w, h )
            res[y,x] = 255 * min(ind1,ind2)
            pass

    cv2.imshow('res', res)
    cv2.imshow('img', img)
    cv2.waitKey(0)
    return


if __name__ == '__main__':
    main( )
