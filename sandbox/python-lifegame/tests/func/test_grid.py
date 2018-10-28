""" Test the Grid class. """

import pytest
from lifegame import Grid, ALIVE

def test_grid():
    grid = Grid(5 , 9)
    grid.assign(0, 3, ALIVE)
    grid.assign(1, 4, ALIVE)
    grid.assign(2, 2, ALIVE)
    grid.assign(2, 3, ALIVE)
    grid.assign(2, 4, ALIVE)

    assert str(grid) == '''\
___*_____
____*____
__***____
_________
_________'''
