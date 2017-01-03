#!/usr/bin/env python
#!-*- coding:utf-8 -*-

side_length = 1001

def main():
	terminal = range(2,side_length + 1,2)

	sum = 1
	b = 1
	for a in terminal:
		sum += 10 * a + 4 * b
		b += 4 * a

	print sum

if __name__ == "__main__":
	main()




