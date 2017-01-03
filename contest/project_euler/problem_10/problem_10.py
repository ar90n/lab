#!/usr/bin/env python3
# -*- coding:utf-8 -*-

def main():
	limit = 2000000
	nums = [ x for x in range(limit + 1) ]

	nums[0] = -1
	nums[1] = -1
	for (i,val) in  enumerate( nums ):
		if( val == -1 ):
			continue
		for j in range( 2 * i,limit + 1,i):
			nums[j] = -1

	res = sum(list( filter( (lambda x: x != -1),nums)))
	print( res  )

if __name__ == "__main__":
	main()





