#!/usr/bin/env python

sum = 0
for i in range(1,  1001 ):
    sum += pow( i, i, 10000000000 )
    sum %= 10000000000

print sum
