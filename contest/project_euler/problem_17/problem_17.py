#!/usr/bin/env python
# -*- cofing:utf-8 -*-
import string


hyaku_dic = [ '','one','two','three','four','five','six','seven','eight','nine']
ju_dic = [ '','','twenty','thirty','forty','fifty','sixty','seventy','eighty','ninety']
ten_dic = [ 'ten','eleven','twelve','thirteen','fourteen','fifteen','sixteen','seventeen','eighteen','nineteen']
ichi_dic = [ '','one','two','three','four','five','six','seven','eight','nine']
def main():
	word_num_list = [ num2word( x ) for x in range( 1,1001 ) ]

	lens = []
	for word in word_num_list:
		tmp = list(filter((lambda x:(x != ' ') and ( x != '-')),word))
		lens.append( len( list(tmp)))
	for (x,y) in zip (lens,word_num_list):
		print x,''.join(y)
	print sum(lens)


def num2word( num ):
	res = [] 
	if( num == 1000 ):
		res += "one thousand"
	elif( num > 99 ) :
		hyaku = num / 100
		ju = ( num % 100 ) / 10
		ichi = num % 10
	
		if( hyaku > 0 ) :
			res += hyaku_dic[ hyaku ] + ' ' + 'hundred'
			if( ( ju > 0 ) or ( ichi > 0 ) ) :
				res += ' and '
		if( ju > 1 ) :
			res += ju_dic[ ju ]
			if( ichi > 0 ) :
				res += '-' + ichi_dic[ ichi ]
		elif( ju > 0 ):
			res += ten_dic[ichi]
		else:
			res += ichi_dic[ ichi ]
	elif( num > 9 ):
		ju = ( num % 100 ) / 10
		ichi = num % 10
	
		if( ju > 1 ) :
			res += ju_dic[ ju ]
			if( ichi > 0 ) :
				res += '-' + ichi_dic[ ichi ]
		elif( ju > 0 ):
			res += ten_dic[ichi]
		else:
			res += ichi_dic[ ichi ]
	else:
		ichi = num % 10
		res += ichi_dic[ ichi ]
	return res
if __name__ == "__main__":
	main();



