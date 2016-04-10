#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def main():
	for b in range(293,1000):
		if( (( 500000 - 1000 * b ) % ( 1000 - b ) ) != 0 ):
			continue
		a = ( 500000 - 1000 * b ) / ( 1000 - b )
		c = 1000 - a - b
		if( (a*a + b * b) == (c * c)):
			break
	print(a,b,c,a*b*c)



if __name__ == "__main__":
	main()

