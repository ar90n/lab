#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import numpy as np
import sklearn
from sklearn import datasets
from matplotlib import pyplot as plt

def main():
    teach_data, teach_class = sklearn.datasets.make_classification(n_features=2, n_redundant=0, n_informative=1, n_clusters_per_class=1)
    teach_class[ teach_class == 0 ] = -1

    one_vector = np.ones( ( teach_data.shape[0],1 ) )
    hom_teach_data = np.concatenate( ( teach_data, one_vector ), axis=1 )
    w = np.linalg.solve( ( hom_teach_data.T.dot( hom_teach_data ) ), ( hom_teach_data.T.dot( teach_class)))

    xmin = min( teach_data[:,0])
    ymin = ( w[0] * xmin + w[2] ) / w[ 1 ]
    xmax = max( teach_data[:,0] )
    ymax = ( w[0] * xmax + w[2] ) / w[ 1 ]
    plt.plot( [xmin, xmax], [ ymin, ymax ] )
    plt.scatter( teach_data[:,0], teach_data[:,1], c=teach_class)
    plt.show()
    return

if __name__ == '__main__':
    main()

