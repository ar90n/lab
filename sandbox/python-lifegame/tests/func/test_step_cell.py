""" Test the step_cell API function. """

import pytest
from lifegame import step_cell, ALIVE, EMPTY

def test_step_cell():
    it = step_cell(10, 5)
    q0 = next(it)
    assert q0.y == 10
    assert q0.x == 5

    q1 = it.send(ALIVE)
    assert q1.y == 11
    assert q1.x == 5

    q2 = it.send(ALIVE)
    assert q2.y == 11
    assert q2.x == 6

    q3 = it.send(ALIVE)
    assert q3.y == 10
    assert q3.x == 6

    q4 = it.send(ALIVE)
    assert q4.y == 9
    assert q4.x == 6

    q5 = it.send(ALIVE)
    assert q5.y == 9
    assert q5.x == 5

    q6 = it.send(EMPTY)
    assert q6.y == 9
    assert q6.x == 4

    q7 = it.send(EMPTY)
    assert q7.y == 10
    assert q7.x == 4

    q8 = it.send(ALIVE)
    assert q8.y == 11
    assert q8.x == 4

    t1 = it.send(EMPTY)
    assert t1.y == 10
    assert t1.x == 5
    assert t1.state == EMPTY