#!/usr/bin/env python
# -*- cofing:utf-8 -*-

#40C20
def main():
	num = 1
	denom = 1
	for i in range(1,20+1):
		num *= (i+20)
		denom *= i

	print( num / denom )

if __name__ == "__main__":
	main();



