#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import math

def factors( n ):
	facts = [1]
	limit = math.ceil( math.sqrt( n ) )

	for i in range( 2, limit):
		if ( n % i ) == 0 :
			facts.append( i )
			facts.append( int( n / i ) )

	if ( n > 1 ) and ( n == ( limit * limit ) ) :
		facts.append( limit )

	facts.sort()
	return facts

def main():
	max_val = 28123
	eval_range = range( 1, max_val + 1 )
	facts = [ factors( x ) for x in eval_range ] 

	abundant = []
	for (i,val) in zip( eval_range, facts ):
		if i < sum( val ) :
			abundant.append( i )


	can_be_written = []
	for (i,x) in enumerate( abundant ):
		can_be_written.extend( [ x + y for y in abundant[ i : ] if x + y <= max_val ] )

	cannot_be_written_flag = [0] * ( max_val + 1 )
	for i in can_be_written:
		cannot_be_written_flag[i] = 1

	print( cannot_be_written_flag[30] )

	cannot_be_written = [ ]
	for (i,val) in enumerate(cannot_be_written_flag ):
		if( val == 0 ):
			cannot_be_written.append( i )


#	cannot_be_written = [ x for x in eval_range if not (x in can_be_written) ]

	print( sum(cannot_be_written) )

if __name__=="__main__":
	main()

