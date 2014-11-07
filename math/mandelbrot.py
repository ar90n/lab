#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt

def main():
    N_max = 50
    some_threshold = 50

    xx = np.linspace( -2, 1, 128 )
    yy = np.linspace( -1.5, 1.5, 128 )
    x,y = np.meshgrid( xx, yy )

    z = 0
    c = x + 1j * y
    for j in xrange( N_max ):
        z = z ** 2 + c

    mask = np.abs( z ) < some_threshold
    plt.imshow( mask )
    plt.show()

    return

if __name__ == '__main__':
    main()
