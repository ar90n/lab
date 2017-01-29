#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import numpy as np
import sklearn
from sklearn import datasets
from matplotlib import pyplot as plt

def main():
    train_data, train_class = sklearn.datasets.make_classification(n_features=2, n_redundant=0, n_informative=2, n_clusters_per_class=2)

    m0 = np.average( train_data[ train_class == 0 ], axis = 0 )
    m1 = np.average( train_data[ train_class == 1 ], axis = 0 )

    tmp0 = train_data[ train_class == 0 ] - m0
    s0 = tmp0.T.dot( tmp0 )
    tmp1 = train_data[ train_class == 1 ] - m1
    s1 = tmp1.T.dot( tmp1 )
    s = s0 + s1

    a = np.linalg.solve( s, m0 - m1 )

    y0 = a.dot( train_data[ train_class == 0 ].T ) 
    y1 = a.dot( train_data[ train_class == 1 ].T ) 

    print( np.average( y0 ) )
    print( np.std( y0 ) )
    print( np.average( y1 ) )
    print( np.std( y1 ) )
    return

if __name__ == '__main__':
    main()

