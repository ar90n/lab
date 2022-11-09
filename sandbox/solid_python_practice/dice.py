# %%
from enum import Enum

from solid import *
from solid.utils import *

class HolePattern(Enum):
    ONE = (1 << 0)
    TWO = (1 << 1) | (1 << 4)
    THREE = (1 << 0) | (1 << 1) | (1 << 4)
    FOUR = (1 << 1) | (1 << 3) | (1 << 4) | (1 << 6)
    FIVE = (1 << 0) | (1 << 1) | (1 << 3) | (1 << 4) | (1 << 6)
    SIX = (1 << 1) | (1 << 2) |  (1 << 3) | (1 << 4) | (1 << 5 ) | (1 << 6)

def get_hole_location(ind: int) -> P3:
    return [
        (0, 0, 0),
        (-2.5, -2.5, 0),
        (-2.5, 0, 0),
        (-2.5, 2.5, 0),
        (2.5, 2.5, 0),
        (2.5, 0, 0),
        (2.5, -2.5, 0),
    ][ind]

def get_holes(pattern: HolePattern):
    holes = None
    for i in range(7):
        r = 2 if pattern == HolePattern.ONE else 1.5
        if pattern.value & (1 << i):
            if holes is None:
                holes = translate(get_hole_location(i))(sphere(r=r, segments=32))
            else:
                holes += translate(get_hole_location(i))(sphere(r=r,segments=32))
    return holes

dice = color("white")(cube( 10, center=True ))
dice = dice - up(6.2)(color("red")(get_holes(HolePattern.ONE)))
dice = dice - down(6.2)(color("black")(get_holes(HolePattern.SIX)))
dice = dice - right(6.2)(rotate(90.0, (0, 1, 0))(color("black")(get_holes(HolePattern.TWO))))
dice = dice - left(6.2)(rotate(90.0, (0, 1, 0))(color("black")(get_holes(HolePattern.FIVE))))
dice = dice - forward(6.2)(rotate(90.0, (1, 0, 0))(color("black")(get_holes(HolePattern.THREE))))
dice = dice - back(6.2)(rotate(90.0, (1, 0, 0))(color("black")(get_holes(HolePattern.FOUR))))
scad_render_to_file(dice, filepath='dice.scad', include_orig_code=True)