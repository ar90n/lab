#!/usr/bin/env python3
# -*- coding:utf-8 -*-

def main():
	primes = []

	i = 2
	while( len( primes ) < 10001 ):
		try:
			for j in primes:
				if( ( i % j ) == 0 ):
					raise non_prime
			primes.append( i )
		except:
			pass
		i += 1
	print( primes )

if __name__ == "__main__":
	main()





