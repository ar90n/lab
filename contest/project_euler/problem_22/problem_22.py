#!/usr/bin/env python
# -*- cofing:utf-8 -*-
import sys

def main():
	names = [ x[1:-1] for x in sys.stdin.read().split(',') ]
	names.sort()

	score = 0
	for (i,name) in enumerate( names ):
		score += sum([ ord( x ) - ord( 'A' ) + 1 for x in list( name ) ] ) * ( i + 1 ) 

	print( score ) 
if __name__ == "__main__":
	main();

