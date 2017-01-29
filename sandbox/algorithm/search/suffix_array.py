#!/usr/bin/env python3
# -*- coding:utf-8 -*-

def main():
    text = '''When in the Course of human events, it becomes necessary for one people to dissolve the political bands which have connected them with another, and to assume among the powers of the earth, the separate and equal station to which the Laws of Nature and of Nature's God entitle them, a decent respect to the opinions of mankind requires that they should declare the causes which impel them to the separation.'''

    pattern = 'in'

    idx = range( len( text ) )
    idx.sort( key = lambda x:( text[x] ) )

    #ternary quick sort 
    valid = len( idx )
    for i,w in enumerate( pattern ):
        l = 0
        pl = 0
        r = len( idx[0:valid] ) - 1
        pr = len( idx[0:valid] ) - 1

        while True:
            while l < r:
                if w < text[ idx[l] + i ]:
                    break
                elif w == text[idx[l] + i]:
                    tmp = idx[l]
                    idx[l] = idx[pl]
                    idx[pl] = tmp
                    pl += 1
                l += 1
            while l < r:
                if text[ idx[r] + i] < w:
                    break
                elif w == text[idx[r] + i]:
                    tmp = idx[r]
                    idx[r] = idx[pr]
                    idx[pr] = tmp
                    pr -= 1
                r -= 1

            tmp = idx[l]
            idx[l] = idx[r]
            idx[r] = tmp

            if l == r:
                break

        while pr < valid:
            tmp = idx[ pl]
            idx[pl] = idx[pr]
            idx[pr] = tmp
            pl += 1
            pr += 1
        valid = pl

    res = idx[0:valid - 1]
    print( res )
    return

if __name__ == '__main__':
    main()
