#!/usr/bin/env python
# -*- cofing:utf-8 -*-

def kaijo( n ):
	if( n != 0 ) :
		return n * kaijo( n - 1 )
	else :
		return 1L

def main():
	res = sum([ int(x) for x in list(str( kaijo( 100 ) ))])
	print(res)

if __name__ == "__main__":
	main();

