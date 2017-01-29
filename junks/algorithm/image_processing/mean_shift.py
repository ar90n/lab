#!/usr/bin/env python3
# -*- coding:utf-8 -*-
from __future__ import division
import matplotlib
matplotlib.use('Qt4Agg')
import cv2
import numpy as np
from mpl_toolkits.axes_grid1 import host_subplot
import mpl_toolkits.axisartist as AA

frame = None
track_point = ( 0, 0 )

def set_track_point( event, x, y, flags, param ):
    global track_point
    global mu

    if event == cv2.EVENT_LBUTTONDOWN:
        track_point = (x,y)

def gauss_dist( x, mu, sigma ):
    xx = ( x - mu ) * ( x - mu )
    var = sigma * sigma
    y = 1.0 / np.sqrt( 2 * np.pi * var ) * np.exp( - xx / var )
    return y

def mean_shift( prob, pos, radius ):
    swap_shape = ( prob.shape[1], prob.shape[0] )
    lt = [ min( m, max( 0, p - radius ) ) for m,p in zip( swap_shape, pos ) ]
    rb = [ min( m, max( 0, p + radius ) ) for m,p in zip( swap_shape, pos ) ]
    print( lt, rb )
    roi = prob[lt[1]:rb[1],lt[0]:rb[0]]

    M = cv2.moments( roi )
    tx = int( M['m10'] / M['m00'] )
    ty = int( M['m01'] / M['m00'] )
    return ( tx + lt[0], ty + lt[1] )

def main():
    global track_point
    global frame
    global mu

    cv2.namedWindow( 'frame' )
    cv2.setMouseCallback( 'frame', set_track_point )

    x = np.arange( 256 )
    sigma = 5

    hh = None
    cap = cv2.VideoCapture( 0 )
    while True:
        ret, frame = cap.read()
        hsv = cv2.cvtColor( frame, cv2.COLOR_BGR2HSV )
        h,s,v = cv2.split( hsv )

        mu = h[ track_point[1] ][ track_point[0] ]
        dist = gauss_dist( x, mu, sigma )

        h = cv2.LUT( h, dist )
        if hh is None:
            hh = h
        else:
            hh = ( hh + h  ) / 2.0

        track_point = mean_shift( hh, track_point, 48 )
        cv2.circle( frame, track_point, 5, ( 255, 0, 0 ), 1 )

        cv2.imshow( 'frame', frame )
        cv2.imshow( 'hh', hh )

        if cv2.waitKey(1) & 0xff == ord('q'):
            break;

    cap.close()
    cv2.destroyAllWindows()
    return

if __name__ == '__main__':
    main()
