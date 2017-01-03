#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import math

def factors( n ):
	facts = [1]
	limit = math.ceil( math.sqrt( n ) )

	for i in range( 2, limit):
		if ( n % i ) == 0 :
			facts.append( i )
			facts.append( int( n / i ) )

	if ( n > 1 ) and ( n == ( limit * limit ) ) :
		facts.append( limit )

	facts.sort()
	return facts

def main():
	amicate_numbers = []
	for a in range(10000):
		b = sum( factors( a ) )
		c = sum( factors( b ) )
		if( a == c ) and ( a != b ):
			amicate_numbers.append(a)

	print( sum( amicate_numbers ))

if __name__=="__main__":
	main()

