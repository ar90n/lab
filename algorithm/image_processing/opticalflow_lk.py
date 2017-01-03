#!/usr/bin/env python3
# -*- coding:utf-8 -*-
from __future__ import division

import numpy as np
import cv2

track_point = ( 0, 0 )

def opticalflow_lk( prev_img, current_img, track_point, params ):
    prev_gray_img = cv2.cvtColor( prev_img, cv2.COLOR_BGR2GRAY )
    prev_smoothed_img1 = cv2.blur( prev_gray_img, (3,3) )
    prev_smoothed_img2 = cv2.blur( prev_smoothed_img1, (3,3) )
    prev_smoothed_img1 = cv2.blur( prev_smoothed_img2, (3,3) )

    current_gray_img = cv2.cvtColor( current_img, cv2.COLOR_BGR2GRAY )
    current_smoothed_img1 = cv2.blur( current_gray_img, (3,3) )
    current_smoothed_img2 = cv2.blur( current_smoothed_img1, (3,3) )
    current_smoothed_img1 = cv2.blur( current_smoothed_img2, (3,3) )

    hor_grad = cv2.Sobel( prev_smoothed_img1, cv2.CV_64F, 0, 1, ksize=1)
    ver_grad = cv2.Sobel( prev_smoothed_img1, cv2.CV_64F, 1, 0, ksize=1)
    tem_grad = np.array( prev_smoothed_img1, dtype = np.float64 ) - np.array( current_smoothed_img1, dtype = np.float64 )

    block_rad = params['block_size'] // 2
    roi = [ ( track_point[1] + dy, track_point[0] + dx ) for dx in range( -block_rad, block_rad + 1 ) for dy in range( -block_rad, block_rad ) ]

    a = np.array( [ [ hor_grad[ pos ], ver_grad[ pos ] ] for pos in roi ] )
    b = np.array( [ tem_grad[ pos ] for pos in roi ] )

    iata = np.linalg.pinv( np.dot( a.T, a ) )
    atb = np.dot( a.T, b )
    v,u = np.dot( iata, atb )

    return ( track_point[0] + u, track_point[1] + v )

def opticalflow_lk_pyr( prev_img, current_img, track_point, params ):
    prev_pyr_img = [ prev_img ]
    current_pyr_img = [ current_img ]
    pyr_track_point = track_point
    for l in range( params['pyr_level'] ):
        prev_parent_img = prev_pyr_img[ l ]
        prev_child_img = cv2.resize( prev_parent_img, ( prev_parent_img.shape[1] // 2, prev_parent_img.shape[0] // 2 ) )
        prev_pyr_img.append( prev_child_img )

        current_parent_img = current_pyr_img[ l ]
        current_child_img = cv2.resize( current_parent_img, ( current_parent_img.shape[1] // 2, current_parent_img.shape[0] // 2 ) )
        current_pyr_img.append( current_child_img )

        pyr_track_point = ( pyr_track_point[0] / 2, pyr_track_point[1] / 2 )

    for prev, current in reversed( zip( prev_pyr_img[1:], current_pyr_img[1:] ) ):
        tmp_track_point = opticalflow_lk( prev, current, pyr_track_point, params )
        pyr_track_point = ( tmp_track_point[0] * 2, tmp_track_point[1] * 2 )
    pyr_track_point = opticalflow_lk( prev_pyr_img[0], current_pyr_img[0], pyr_track_point, params )

    return pyr_track_point

def set_track_point( event, x, y, flags, param ):
    global track_point
    if event == cv2.EVENT_LBUTTONDOWN:
        track_point = (x,y)

def main():
    global track_point

    cv2.namedWindow( 'frame' )
    cv2.setMouseCallback( 'frame', set_track_point )

    of_params = {
        'block_size' : 5,
        'pyr_level'  : 0
    }

    cap = cv2.VideoCapture(0)
    ret, prev_img = cap.read()
    while(True):
        ret, frame = cap.read()
        current_img = np.copy( frame )

        track_point = tuple( [ int(x + 0.5) for x in opticalflow_lk_pyr( prev_img, current_img, track_point, of_params ) ] )
        cv2.circle( frame, track_point, 5, (0,0,255), -1 )

        cv2.imshow("frame", frame)
        prev_img = current_img

        if cv2.waitKey( 1 ) & 0xff == ord('q'):
            break
    return

if __name__ == '__main__':
    main()
