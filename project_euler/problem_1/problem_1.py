#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys

def main():
	max_val = int(sys.argv[1])
	print( sum([ x for x in range(max_val ) if ( x % 3 == 0 ) or ( x % 5 == 0 ) ] ))

if __name__ == "__main__":
	main()
