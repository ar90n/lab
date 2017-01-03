#!/usr/bin/env python3
# -*- coding:utf-8 -*-



def main():
	target = 600851475143

	facts = []
	for i in range(2,target):
		try:
			for j in range(2,i):
				if( ( i % j ) == 0 ):
					raise non_prime
			if( ( target % i ) == 0 ):
				target /= i
				facts.append( i )
		except:
			pass
		if( target == 1 ):
			break;

	print( facts )

if __name__ == "__main__":
	main()





