#!/usr/bin/env python2
# -*- coding:utf-8 -*-

from __future__ import division

import sys
import cv2
import numpy as np
import scipy as sp
import scipy.ndimage

def harris_feature_extraction( img, sigma, min_dst = 10, thresh = 0.1 ):
    #ix = cv2.Sobel( img, cv2.CV_64F, 1, 0, ksize = 3 )
    #iy = cv2.Sobel( img, cv2.CV_64F, 0, 1, ksize = 3 )
    ix = np.zeros( img.shape )
    iy = np.zeros( img.shape )
    sp.ndimage.filters.gaussian_filter( img, (sigma,sigma), (0,1), ix )
    sp.ndimage.filters.gaussian_filter( img, (sigma,sigma), (1,0), iy )

    wxx = sp.ndimage.filters.gaussian_filter( ix * ix , sigma )
    wxy = sp.ndimage.filters.gaussian_filter( ix * iy , sigma )
    wyy = sp.ndimage.filters.gaussian_filter( iy * iy , sigma )

    wdet = wxx * wyy - wxy * wxy
    wtr = wxx + wyy
    harris_img = wdet / (wtr + 0.00001 )

    corner_threshold =  harris_img.max() * thresh
    coords = np.array( (harris_img > corner_threshold).nonzero() ).T
    candidate_values = [ harris_img[ c[0],c[1] ] for c in coords ]

    index = np.argsort( candidate_values )

    allowed_locations = np.zeros( harris_img.shape )
    allowed_locations[ min_dst: -min_dst, min_dst: -min_dst ] = 1

    filtered_coords = []
    for i in index:
        if allowed_locations[ coords[i,0], coords[i,1] ] == 1:
            filtered_coords.append( coords[i] )
            allowed_locations[ coords[i,0] - min_dst : coords[i,0] + min_dst, coords[i,1] - min_dst : coords[i,1] + min_dst ] = 0

    return filtered_coords

def main():
    try:
        img1 = cv2.imread( sys.argv[1] )
    except:
        sys.exit(-1)

    gray1 = cv2.cvtColor( img1, cv2.COLOR_BGR2GRAY )
    points = harris_feature_extraction( gray1, 0.5 )
    for p in points:
        cv2.circle( img1, (p[1],p[0]), 1, ( 0,0,255 ), -1 )

    cv2.imshow( 'img1', img1 )
    cv2.waitKey(0)
    cv2.destroyAllWindows()

    return

if __name__ == '__main__':
    main()
