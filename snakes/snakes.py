#!/usr/bin/env python
#-*- coding:utf-8 -*-

from __future__ import division
import sys
import cv
import math


node_list =[]
cv_node_list =[]

def wbCallBack( event, x, y, flags, param ):
    global node_list

    if event == cv.CV_EVENT_LBUTTONUP:
        node_list.append( (x, y ) )

def calc_neighbor_pos( n, pos ):
    return map( lambda x : (pos[0] + x[0], pos[1]+x[1]), [(a, b) for a in xrange(-n, (n + 1 )) for b in xrange(-n, (n+1))])

def get_neighbor_pixel( img, n, pos ):
    neigthbor_pixel = []
    for x, y in calc_neighbor_pos( n, pos ):
        try:
            neigthbor_pixel.append( cv.Get2D( img, y, x )[0] )
        except:
            neigthbor_pixel.append( cv.Get2D( img, pos[1], pos[0] )[0] )

    return neigthbor_pixel

def snakes( img, nodes, alpha, beta, gammga, size ):
    n = len( nodes )
    neighbor_pos_list = map( lambda x :calc_neighbor_pos( size, x ) , nodes )

    average_distance = 0
    for i in range( n ) :
        diff_x = nodes[i][0] - nodes[ (i + 1) % n ][0]
        diff_y = nodes[i][1] - nodes[ (i + 1) % n ][1]

        average_distance += math.sqrt( diff_x * diff_x + diff_y * diff_y )
    average_distance /= n

    count_energy = []
    curve_energy = []
    image_energy = []
    for i in range( n ):
        image_energy.append( map( lambda x : calc_image_energy( img, x ), neighbor_pos_list[i] ) )
        count_energy.append( map( lambda x: calc_count_energy( img, average_distance, x ) ,
            zip( neighbor_pos_list[i],
                 [ neighbor_pos_list[ ( i + 1 ) % n ][ int( len( neighbor_pos_list[ ( i - 1 )  % n ] ) / 2 ) ] ] * len( neighbor_pos_list[i] ) ) ) )
        curve_energy.append( map( lambda x: calc_curve_energy( img, x ) ,
            zip( [ neighbor_pos_list[ ( i - 1 ) % n ][ int( len( neighbor_pos_list[ ( i - 1 ) % n  ] ) / 2 ) ] ] * len( neighbor_pos_list[i] ),
                 neighbor_pos_list[i],
                 [ neighbor_pos_list[ ( i + 1 ) % n ][ int( len( neighbor_pos_list[ ( i - 1 )  % n ] ) / 2 ) ] ] * len( neighbor_pos_list[i] ) ) ) )

    for i in range( n ):
        max_energy = max( count_energy[i] )
        count_energy[i] = map( lambda x : x / max_energy, count_energy[i] )

        max_energy = max( curve_energy[i] )
        curve_energy[i] = map( lambda x : x / max_energy, curve_energy[i] )

    local_energy = []
    for i in range( n ):
        local_energy.append( map( lambda x: alpha * x[0] + beta * x[1] + gamma * x[2] , zip( count_energy[i], curve_energy[i], image_energy[i] ) ) )

    min_local_energy = map( min, local_energy )

    return [ x[0][x[1]] for x in zip( neighbor_pos_list, map( lambda x: x.index( min( x ) ) ,  local_energy ) ) ]

def calc_count_energy( img, average,  pos_list ):
    pos_x = [ pos[0] for pos in pos_list ]
    pos_y = [ pos[1] for pos in pos_list ]

    return  pow( average - math.sqrt( pow( pos_x[1] - pos_x[0] , 2 ) + pow( pos_y[1] - pos_y[0], 2 ) ), 2 )

def calc_curve_energy( img, pos_list ):
    pos_x = [ pos[0] for pos in pos_list ]
    pos_y = [ pos[1] for pos in pos_list ]

    return pow( pos_x[2] - 2 * pos_x[1] + pos_x[0] , 2 ) +  pow( pos_y[2] - 2 * pos_y[1] + pos_y[0] , 2 )

def calc_image_energy( img, center_pos ):
    try:
        center_pixel = int( cv.Get2D( img, center_pos[1], center_pos[0] )[0] )
    except:
        return 100000000

    neigthbor_pixel = get_neighbor_pixel( img, 1,  center_pos )

    min_pixel = min( neigthbor_pixel )
    max_pixel = max( neigthbor_pixel )

    image_energy = -( center_pixel - min_pixel ) / max( max_pixel - min_pixel, 5 )
    return image_energy



if __name__=='__main__':
    if( len( sys.argv ) < 2):
        sys.exit(-1)

    img = cv.LoadImage( sys.argv[1], cv.CV_LOAD_IMAGE_GRAYSCALE )

    smooth_img = cv.CreateImage( cv.GetSize( img ), cv.IPL_DEPTH_8U , 1 )
    grad_img = cv.CreateImage( cv.GetSize( img ),  cv.IPL_DEPTH_16S , 1 )
    tmp_img1 = cv.CreateImage( cv.GetSize( img ), cv.IPL_DEPTH_8U, 3 )
    tmp_img2 = cv.CreateImage( cv.GetSize( img ), cv.IPL_DEPTH_8U, 1 )

    cv.Smooth( img, smooth_img, cv.CV_GAUSSIAN,3  )
    cv.Sobel( smooth_img, grad_img, 1, 1)
    cv.ConvertScaleAbs( grad_img, tmp_img2, 1, 0 )

    node_num = 50;
    for i in range( node_num ):
        x = int( 250 + 120 * math.cos( 2 * math.pi * i / node_num ) )
        y = int( 250 + 120 * math.sin( 2 * math.pi * i / node_num ) )
        node_list.append( (x, y) )
        cv_node_list.append( (x, y) )


    cv.NamedWindow( "Snake Test!!", 1 )
    cv.SetMouseCallback( "Snake Test!!", wbCallBack )

    alpha = 0.20;
    beta = 0.25;
    gamma = 0.2;
    color = cv.Scalar( 0, 0, 255 )
    cv_color = cv.Scalar( 0, 255, 0)
    while( True ):
        cv.CvtColor( img, tmp_img1, cv.CV_GRAY2BGR )

        for node_center in node_list:
            cv.Circle( tmp_img1, node_center, 5, color, -1 )
        cv.PolyLine( tmp_img1, [ node_list ], True, color, 2 )

        for node_center in cv_node_list:
            cv.Circle( tmp_img1, node_center, 5, cv_color, -1 )
        cv.PolyLine( tmp_img1, [ cv_node_list ], True, cv_color, 2 )

        cv.ShowImage( "Snake Test!!", tmp_img1 )

        hit_key = cv.WaitKey( 1 )
        if( hit_key == ord('q') ):
            sys.exit( -1 )
        elif( hit_key == ord('c')):
            node_list = []
        elif( hit_key == ord(' ')):
            node_list = snakes( tmp_img2, node_list, alpha, beta, gamma, 1 )
            cv_node_list = cv.SnakeImage( tmp_img2, cv_node_list, alpha, beta, gamma, ( 3, 3 ), (cv.CV_TERMCRIT_ITER, 100, 0.0 ) )


    cv.DestroyWindow( "Snake Test!!" )


