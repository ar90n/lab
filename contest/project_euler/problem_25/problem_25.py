#!/usr/bin/env python
# -*- cofing:utf-8 -*-
import math

def fibo( n ):
	golden_ratio = ( 1 + math.sqrt( 5 ) ) / 2
	return int( math.floor( math.pow( golden_ratio , n ) / math.sqrt( 5 ) + 0.5 ) )

def digits( n ):
	return len( str( n ) )

def main():
	val0 = fibo( 1100 )
	val1 = fibo( 1101 )
	index = 1101
	while( digits( val1 ) < 1000 ):
		tmp = val1
		val1 = val1 + val0
		val0 = tmp
		index += 1
	print index
if __name__ == "__main__":
	main();

