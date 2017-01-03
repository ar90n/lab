#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def main():
	limit = 4000000
	state = [ 2,1 ]
	sum = 2
	while( state[0] < limit ):
		state[0] = state[0] + state[1]
		state[1] = state[0] - state[1]
		if( (state[0] % 2) == 0):	
			sum += state[0]
	print( sum )


if __name__ == "__main__":
	main()



