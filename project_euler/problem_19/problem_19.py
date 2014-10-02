#!/usr/bin/env python
# -*- cofing:utf-8 -*-

def zeller( year, month, day ):
	tmp = year + year / 4 + year / 100 + ( 13 * month + 8 ) / 5 + day
	return tmp % 7

def main():
	count = 0
	for year in range( 1901,2001 ):
		for month in range(1,13):
			if zeller( year, month, 1 ) == 0 :
				count += 1
	print(count )

if __name__ == "__main__":
	main();

