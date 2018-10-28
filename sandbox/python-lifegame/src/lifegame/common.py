from collections import namedtuple

ALIVE = '*'
EMPTY = '_' 
TICK = object

Query = namedtuple('Query', ('y', 'x'))
Transition = namedtuple('Transition', ('y', 'x', 'state'))