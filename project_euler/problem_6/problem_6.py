#!/usr/bin/env python3
# -*- coding:utf-8 -*- 

def main():
	limit = 100
	val1 = sum([ x * x for x in range(limit + 1 ) ])
	val2 = sum([ x for x in range(limit + 1 ) ])
	val2 = val2 * val2

	print( val2 - val1 )

if __name__ == "__main__":
	main()
