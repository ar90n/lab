#!/usr/bin/env python
# -*- coding:utf-8 -*-
from __future__ import division
import sys
import scipy as sp
import numpy as np
import matplotlib.pyplot as  plt
import matplotlib.cm as  cm

def isNear( dist, rad ):
    return ( 0.0 < dist ) and ( dist < rad )

def checkNears( data, allData, r ):
    return  sp.array( map( lambda x : isNear( sp.dot( x , x.transpose() ), r )  , allData - data ) )

def getNears( data, allData, r ):
    return allData[ checkNears( data, allData, r ) ]

conv_crit = 0.00000001
rad =  0.8
def main():
    data = map( lambda x: x.split(',')[0:-1], open( sys.argv[1] ).read().split('\n') )
    data = sp.array( [ map( float, x ) for x in data ][0:-1] )[:,[2,3]]

    #data = map( lambda x: x.split(' '), open( sys.argv[1] ).read().split('\r\n') )
    #data = sp.array( [ map( float, x ) for x in data[0:-1] ] )

    w,h = data.shape
    conv_points = []
    print sp.tile(data,(1,1,w),2)
    print w,h
    for num,ddd in enumerate(sp.tile( data, (1,w),) ):
        current_data = ddd[ num ]
        print current_data
        sys.exit()
        last_data = current_data

        nears = getNears( current_data, data, rad )
        if nears.size != 0:
            current_data = sp.average( nears, 0  )

        while( conv_crit < np.linalg.norm( current_data - last_data ) ):
            last_data = current_data
            nears = getNears( current_data, data, rad )
            if nears.size != 0:
                current_data = sp.average( nears, 0  )
        #conv_points.append( current_data )
        conv_points.append( tuple(current_data) )

    cl = []
    classes = list(set(conv_points))
    clnum = len( classes )
    for conv in conv_points:
        cl.append( classes.index( tuple( conv )) )

    options = ["bo", "ro", "go", "co", "mo", "yo", "ko"]
    for i,v in enumerate( data ):
        plt.plot( v[0],v[1],'o',c=cm.rainbow( cl[i] / clnum))
    plt.show()
    return

if __name__ == '__main__':
    main()
