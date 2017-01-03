#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import numpy as np
import sklearn
from sklearn import datasets
from matplotlib import pyplot as plt

def main():
    train_data, train_class = sklearn.datasets.make_classification(n_features=2, n_redundant=0, n_informative=1, n_clusters_per_class=1)
    train_class[ train_class == 0 ] = -1

    one_vector = np.ones( ( train_data.shape[0],1 ) )
    hom_train_data = np.concatenate( ( train_data, one_vector ), axis=1 )

    rho = 0.1
    criteria = 1e-20
    w = np.random.rand( 3 )
    while True:
        last_w = w
        for curr_data,curr_class in zip( hom_train_data, train_class ) :
            w -= rho * ( w.T.dot( curr_data ) - curr_class ) * curr_data
        if np.linalg.norm(w - last_w) < criteria:
            break;

    lamb = 0.00001
    xmin = min( train_data[:,0])
    ymin = ( w[0] * xmin + w[2] ) / ( w[ 1 ] + lamb )
    xmax = max( train_data[:,0] )
    ymax = ( w[0] * xmax + w[2] ) / ( w[ 1 ] + lamb )
    plt.plot( [xmin, xmax], [ ymin, ymax ] )

    plt.scatter( train_data[:,0], train_data[:,1], c=train_class)
    plt.show()
    return

if __name__ == '__main__':
    main()

