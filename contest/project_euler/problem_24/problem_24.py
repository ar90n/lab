#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import math

def kaijo( n ):
	if( n == 0 ):
		return 1
	else:
		return n * kaijo( n - 1 )

def main():
	alphabet = list("0123456789")
	target_index = 1000000 - 1

	result = []
	for i in range( len(alphabet) - 1,0,-1):
		number_of_perm = kaijo( i )
		idx =  int( math.floor( target_index / number_of_perm ) )  
		target_index = target_index % number_of_perm

		item = alphabet[ idx ]
		result.append( item )
		alphabet.remove( item )

	result.append( alphabet[0] )

	print( ''.join(result) )

if __name__=="__main__":
	main()

