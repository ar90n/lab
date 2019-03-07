#!/usr/bin/env python
# -*- coding:utf-8 -*-
import cv2

def main():
    lena = cv2.imread( 'lena.jpg' )
    cv2_meanshift_lena = cv2.pyrMeanShiftFiltering( lena, 16 , 32 )

    cv2.imshow( 'cv2_meashift_lena', cv2_meanshift_lena )
    cv2.waitKey()
    return

if __name__ == '__main__':
    main()


