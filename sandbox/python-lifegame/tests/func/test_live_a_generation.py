""" Test the live_a_generation API function. """

import pytest
from lifegame import live_a_generation, Grid, simulate, ALIVE


def test_live_a_generation():
    grid = Grid(5 , 9)
    grid.assign(0, 3, ALIVE)
    grid.assign(1, 4, ALIVE)
    grid.assign(2, 2, ALIVE)
    grid.assign(2, 3, ALIVE)
    grid.assign(2, 4, ALIVE)

    sim = simulate(5, 9)

    actual = live_a_generation(grid, sim)

    expected = Grid(5 , 9)
    expected.assign(1, 2, ALIVE)
    expected.assign(1, 4, ALIVE)
    expected.assign(2, 3, ALIVE)
    expected.assign(2, 4, ALIVE)
    expected.assign(3, 3, ALIVE)
    assert str(actual) == str(expected)

