#!/usr/bin/env python
# -*- cofing:utf-8 -*-
import math


vals = [ int(x) for x in """75 95 64 17 47 82 18 35 87 10 20 04 82 47 65
19 01 23 75 03 34 88 02 77 73 07 63 67 99 65 04
28 06 16 70 92 41 41 26 56 83 40 80 70 33 41 48
72 33 47 32 37 16 94 29 53 71 44 65 25 43 91 52
97 51 14 70 11 33 28 77 73 17 78 39 68 17 57 91
71 52 38 17 14 91 43 58 50 27 29 48 63 66 04 68
89 53 67 30 73 16 69 87 40 31 04 62 98 27 23 09
70 98 73 93 38 53 60 04 23""".replace('\n',' ').split(' ') ]


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
	main();

