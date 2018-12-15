from abc import ABCMeta

import click

class Base(ABCMeta):
    MSG = 'abc'
    def __repr__(cls):
        return cls.MSG

def _(mes):
    class d(Base):
        MSG = mes
    return d

class Derived(metaclass=_('eee')):
    pass

assert repr(Derived) == 'eee'