#!/usr/bin/env python
# -*- coding:utf-8 -*-

from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import
from future_builtins import *

import cv2
import numpy as np
import networkx as nx

def idx( x, y, i, w, h ):
    return i * w * h + y * w + x

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

    print( nx.maximum_flow( g, 0, 3 ) )
    print( nx.minimum_cut( g, 0, 3 ) )
    return


if __name__ == '__main__':
    main( )
