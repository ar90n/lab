#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import numpy as np

def gauss_jordan_elimination( a ):
    eqns = a.shape[0]
    a = np.concatenate( (a, np.eye( eqns )), axis=1 )

    for i in range( eqns ):
        pipot = np.argmax( a[i:,i] ) + i
        a[[i,pipot],:] = a[[pipot,i],:]

        a[i,:] /= a[i,i]
        for j in range( eqns ):
            if i != j:
                a[j,:] -= a[j,i] * a[i,:] / a[i,i]

    return a[:,eqns:]

def main():
    a = np.array( [ [ 2, 4, 2 ],
                    [ 4,10, 3 ],
                    [ 3, 7, 1 ] ] )
    b1 = np.linalg.inv( a )
    b2 = gauss_jordan_elimination( a )

    print( b1 )
    print( b2 )

    return

if __name__ == '__main__':
    main()
