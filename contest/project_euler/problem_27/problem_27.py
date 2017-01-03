#!/usr/bin/env python
#! -*- cofing:utf-8 -*-

def primes( upper_limit ):
	num_table = [0] * ( upper_limit + 1 );
	
	num_table[0] = 1
	num_table[1] = 1
	for i in range( 2, upper_limit ):
		for j in range( 2 * i, upper_limit+1, i ):
			num_table[ j ] = 1 

	prime_number = [];
	for (i , val ) in enumerate( num_table ):
		if( val == 0 ):
			prime_number.append( i ) 
	
	return prime_number

def is_prime( n ):
	if( n < 0 ):
		return  False
	
	for i in xrange( 2,int(n ** 0.5) + 1 ):
		if( ( n % i ) == 0 ):
			return False

	return True

def prime_generator( a, b, n ):
	return n * n + a * n + b

def prime_consecutiveness( a, b ):
	n = 1
	val = prime_generator( a, b, n )
	while( is_prime( val ) ):
		n = n + 1
		val = prime_generator( a, b, n )
	
	return n

def main():
	prime_numbers = primes( 1000 );
	b_table = prime_numbers + map( lambda x:-x,prime_numbers)

	max_length = 0
	for a in range(-1000,1000):
		for b in b_table:
			tmp = prime_consecutiveness( a, b );
			if( max_length < tmp ):
				coef = ( a, b )
				max_length = tmp
	print max_length
	print coef


if __name__=="__main__":
	main()
		

