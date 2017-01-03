#!/usr/bin/env python
# -*- cofing:utf-8 -*-

def main():
	i =1
	while( True ):
		tri_num = (i * (i + 1))/2;
#		factors = len( [ x  for x in range( 1,tri_num + 1) if ( tri_num % x ) == 0 ] )
		j = 1
		limit = tri_num
		factors = 0
		while( j < limit ):
			if( tri_num % j ) == 0:
				factors += 2
				limit = tri_num / j
			j += 1

		if factors > 500 :
			break
		else:
			i += 1
	print( tri_num )

if __name__ == "__main__":
	main()
