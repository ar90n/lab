#!/usr/bin/env python
# -*- cofing:utf-8 -*-

lens = {}

def collatz( n ):
	if( lens.has_key( n ) ):
		return [0] * lens[n]
	else:
		if( n == 1 ) :
			lens[ n ] = 1
			return [n]
		elif( n % 2 ) == 0 :
			res = [n] + collatz( n / 2 )
			lens[ n ] = len( res )
			return res
		else :
			res = [n] + collatz( 3 * n + 1 )
			lens[ n ] = len( res )
			return res

def main():
	res = [len(collatz(x)) for x in range(1,1000001)]
	idx = res.index(max(res)) + 1
	print(idx)
if __name__ == "__main__":
	main();

