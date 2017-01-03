#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import math

def main():
	primes = [ 2,3,5,7,11,13,17,19 ]
	nums = list(map( (lambda x: math.floor( math.log10( 20 ) / math.log10( x ) )),primes))
	vals = [ math.pow( x, y ) for ( x , y ) in  zip( primes, nums )]

	res = 1
	for val in vals:
		res *= val

	print( res )

if __name__ == "__main__":
	main()


