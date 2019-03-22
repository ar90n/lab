#!/usr/bin/env python
# -*- coding:utf-8 -*-
from __future__ import division
import sys
import scipy as sp
import numpy as np
import matplotlib.pyplot as  plt
import matplotlib.cm as  cm

rad = 3.0#meanshiftのカーネルサイズ
conv_crit = 0.001#収束についての閾値
data_params = [(( 1.0,1.0 ),2.0,150),((10.0,10.0),1.0,150)]#データセットのパラメータ(mu,var,num)

#近傍かどうか
def isNear( dist, rad ):
    return dist < rad

#allDataの各サンプルが近傍かどうかをsp.arrayで返す
def checkNears( data, allData, r ):
    return  sp.array( map( lambda x : isNear( np.linalg.norm(x ), r )  , allData - data ) )

#近傍のサンプルをsp.arrayで返す
def getNears( data, allData, r ):
    return allData[ checkNears( data, allData, r ) ]

#収束点からクラスを計算
def getClasses( conv_points, th ):
    res = [0] * len( conv_points )
    class_count = 0
    index_class_tab = { 0 : 0 }
    for i,conv_point in enumerate( conv_points ):
        tmp = sp.nonzero(checkNears( conv_point, conv_points, th ) == True )[0][0]
        if not index_class_tab.has_key( tmp ):
            class_count += 1
            index_class_tab[ tmp ] = class_count
        res[i] = index_class_tab[ tmp ]
    return sp.array( res ), dict( zip( index_class_tab.values(), index_class_tab.keys()) )

def main():
    #適当にデータをつくる
    for mu,var,num in data_params:
        try:
            data = sp.append(data,sp.random.normal( mu, var, ( num,2 )),0)
        except NameError:
            data = sp.random.normal( mu, var, ( num,2 ))

    #各サンプルに対してmeanshiftを適用する
    conv_points = []#ここに収束点の座標がはいる
    for num,current_data in enumerate(data):
        last_data = current_data

        nears = getNears( current_data, data, rad )
        current_data = sp.average( nears, 0  )
        while( conv_crit < np.linalg.norm( current_data - last_data ) ):
            last_data = current_data
            nears = getNears( current_data, data, rad )
            current_data = sp.average( nears, 0  )

        conv_points.append( current_data )

    #角収束点からクラスを求める
    classes,class_point_tab = getClasses( conv_points, rad )

    #プロットする
    for i,v in enumerate( data ):
        plt.plot( v[0],v[1],'.',c=cm.rainbow( classes[i] / classes.max() ))

    for i,vv in enumerate( list(set(classes)) ):
        v = conv_points[ class_point_tab[ vv ] ]
        plt.plot( v[0],v[1],'o',c=cm.rainbow( i / classes.max() ))

    plt.show()
    return

if __name__ == '__main__':
    main()
