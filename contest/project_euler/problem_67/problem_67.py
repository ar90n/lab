#!/usr/bin/env python
# -*- cofing:utf-8 -*-
import math


vals = [];

def init():
	global vals
	vals = [int( x ) for x in open( "triangle.txt" ).read()[:-2].replace('\r\n', ' ').split(' ')]

def main():
	num = len( vals )
	height = int(math.floor( ( math.sqrt( 8 * num + 1 ) - 1 ) / 2 ))

	sums = [0] * num;
	sums[0] = vals[0]
	for i in range(1,height):
		start_index = int( math.floor( ( i * ( i + 1 ) ) / 2 ) )

		right_index = start_index - i
		sums[ start_index ] = sums[ right_index ] + vals[ start_index ]

		for j in range( start_index + 1, start_index + i  ):
			left_index = j - i - 1
			right_index = j - i
			
			if( sums[ left_index ] > sums[ right_index ] ) :
				sums[ j ] += sums[ left_index ] + vals[ j ]
			else:
				sums[ j ] += sums[ right_index ] + vals[ j ]

		left_index = start_index - 1
		sums[ start_index + i ] = sums[ left_index ] + vals[ start_index + i ]
	print max(sums)

if __name__ == "__main__":
	init();
	main();

