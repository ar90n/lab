#!/usr/bin/env python
from __future__ import division
import scipy as sp
import numpy as np
import matplotlib.pyplot as plt
import math

term_num = 17
thetas =  map( lambda x : math.atan( 1.0 / math.pow( 2.0,x ) ) , range( term_num  + 1) )
hypot_length =  1.0 / reduce( lambda x,y:  x * ( 1.0 / math.cos(y) ) ,thetas,1.0 )

def cos_cordic( angle ):
	x,y = (1.0,1.0)
	acc_theta = thetas[0]
	scale_ratio = 1.0
	for i,theta in enumerate( thetas[1:] ):
		x1,y1 = x,y
		scale_ratio *= 0.5
		if acc_theta < angle :
			acc_theta += theta
			x -= scale_ratio * y1
			y += scale_ratio * x1
		else:
			acc_theta -= theta
			x += scale_ratio * y1
			y -= scale_ratio * x1
	return ( x * hypot_length )


if __name__ == '__main__':
	t = [ x * ( math.pi / 200.0 ) for x in range( 100 ) ]
	result = map( cos_cordic,t )
        print map( lambda x : int(( 1 << 17 ) *x + 0.5) , thetas )
        print int( ( 1 << 17 ) * hypot_length + 0.5 )
#	plt.plot( result )
#	plt.show()

