#!/usr/bin/env python3
# -*- coding:utf-8 -*-

def main():
	palindromic = []

	for i in range( 1, 1001 ):
		for j in range( 1,1001):
			val = str(i * j)
			rval = list(val)
			rval.reverse()
			srval = ''.join(rval)
			if( val == srval ):
				palindromic.append( int(val) )
	
			
	print( max(palindromic ))

if __name__ == "__main__":
	main()



