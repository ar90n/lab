#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import numpy as np
import sklearn
from sklearn import datasets
from matplotlib import pyplot as plt

def calc_network( ws, layer_input ):
    layer_outputs = [ layer_input ]
    for w in ws:
        weight_acc = w.dot(  layer_outputs[-1] )
        tmp = 1.0 / ( 1.0 + np.exp( -weight_acc ) )
        layer_outputs.append( tmp )

    return layer_outputs

def main():
    train_data, train_class = sklearn.datasets.make_classification(n_features=2, n_redundant=0, n_informative=2, n_clusters_per_class=2)

    one_vector = np.ones( ( train_data.shape[0],1 ) )
    hom_train_data = np.concatenate( ( train_data, one_vector ), axis=1 )

    ws = [ np.random.rand( 3, 3 ) for i in range( 1, 3 - 1 ) ]
    ws.append( np.random.rand( 2, 3 ) )

    conv_criteria = 0.3
    rho = 0.8
    while True:
        for curr_data,curr_class in zip( hom_train_data, train_class ) :
            train_class_vector = np.array([[0], [0]] )
            if curr_class == 1:
                train_class_vector[0][0] = 1
            else:
                train_class_vector[1][0] = 1

            layer_input = np.array( [curr_data] ).T
            layer_outputs = calc_network( ws, layer_input )

            curr_layer_outputs = layer_outputs[ -1 ]
            eps = ( curr_layer_outputs - train_class_vector ) * curr_layer_outputs * ( 1.0 - curr_layer_outputs )

            curr_layer_outputs = layer_outputs[ -2 ]
            ws[-1] -= rho * eps.dot( curr_layer_outputs.T )
            eps = ( ws[-1].T.dot( eps ) ) * curr_layer_outputs * ( 1.0 - curr_layer_outputs )

            curr_layer_outputs = layer_outputs[ -3 ]
            eps = ( ws[-2].T.dot( eps ) ) * curr_layer_outputs * ( 1.0 - curr_layer_outputs )

        avg_error = 0.0
        for curr_data,curr_class in zip( hom_train_data, train_class ) :
            output = calc_network( ws, curr_data )
            avg_error += abs( output[-1][1 - curr_class] - 1 ) + abs( output[-1][curr_class] )
        avg_error /= train_class.shape[0]

        if avg_error < conv_criteria:
            break

    xmin = min( train_data[:,0])
    ymin = min( train_data[:,1])
    xmax = max( train_data[:,0] )
    ymax = max( train_data[:,1] )
    x = np.linspace( xmin, xmax, 128 )
    y = np.linspace( ymin, ymax, 128 )
    xv,yv = np.meshgrid( x, y )

    n = xv.shape[0] * xv.shape[1]
    result = np.zeros( (n,1) )
    test_data = np.hstack( ( xv.reshape(n,1), yv.reshape(n,1), np.ones( (n,1) ) ) )
    for i,data in enumerate( test_data ):
        output = calc_network( ws, data )
        if ( output[-1][0] < output[-1][1] ):
            result[i] = 1
        else:
            result[i] = 0
    result = result.reshape( xv.shape, order='C')

    plt.imshow( result, extent=( xmin, xmax, ymax, ymin ) )
    plt.scatter( train_data[:,0], train_data[:,1], c=train_class)
    plt.show()
    return

if __name__ == '__main__':
    main()

