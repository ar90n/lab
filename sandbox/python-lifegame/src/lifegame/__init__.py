from .common import ALIVE, EMPTY, TICK, Query, Transition
from .grid import Grid

def count_neighbors(y, x):
    n_ = yield Query(y + 1, x + 0)
    ne = yield Query(y + 1, x + 1)
    e_ = yield Query(y + 0, x + 1)
    se = yield Query(y - 1, x + 1)
    s_ = yield Query(y - 1, x + 0)
    sw = yield Query(y - 1, x - 1)
    w_ = yield Query(y + 0, x - 1)
    nw = yield Query(y + 1, x - 1)
    neighbor_states = [ n_, ne, e_, se, s_, sw, w_, nw ]

    count = 0
    for state in neighbor_states:
        count += 1 if state == ALIVE else 0
    return count       


def game_logic(state, neighbors):
    """
    This function defined lifegame main logic.

    >>> from lifegame import game_logic, ALIVE, EMPTY
    >>> game_logic(ALIVE, 3)
    '*'
    >>> game_logic(ALIVE, 1)
    '_'
    >>> game_logic(ALIVE, 4)
    '_'
    >>> game_logic(EMPTY, 3)
    '*'
    >>> game_logic(EMPTY, 2)
    '_'
    """
    if state == ALIVE and (neighbors < 2 or 3 < neighbors):
        return EMPTY
    elif neighbors == 3:
        return ALIVE
    return state


def step_cell(y, x):
    state = yield Query(y, x)
    neighbors = yield from count_neighbors(y, x)
    next_state = game_logic(state, neighbors)
    yield Transition(y, x, next_state)


def simulate(height, width):
    while True:
        for y in range(height):
            for x in range(width):
                yield from step_cell(y, x)
        yield TICK


def live_a_generation(grid, sim):
    progeny = Grid(grid.height, grid.width)
    item = next(sim)
    while item is not TICK:
        if isinstance(item, Query):
            state = grid.query(item.y, item.x)
            item = sim.send(state)
        else:
            progeny.assign(item.y, item.x, item.state)
            item = next(sim)
    return progeny
