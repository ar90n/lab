#!/usr/bin/env python
#!-*- coding:utf-8 -*-

limit = 100;

def main():
	result = [0] * ( limit -1 ) ** 2 
	for (i, a) in enumerate( range( 2, limit + 1 ) ):
		for (j,b) in enumerate( range( 2, limit + 1 ) ):
			index = i * (limit - 1) + j
			result[ index ] = a ** b
	
	result = list( set( result ) )
	result.sort()
	print len( result )

if __name__ == "__main__":
	main()




