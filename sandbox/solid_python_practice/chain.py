# %%
import math

from solid import *
from solid.utils import *


def half_torus(r1: float, r2: float):
    total_radius = r1 / 2.0 + r2
    mask = translate((0, -total_radius, -total_radius))(cube(2 * total_radius))
    return mask * rotate_extrude(convexity=10, segments=24)(
        translate((r1 / 2.0, 0.0, 0.0))(circle(r=r2))
    )


def half_link(w: float, r1: float, r2: float):
    arc = translate((w / 2.0, 0, 0))(half_torus(r1, r2))
    shuft = translate((0, r1 / 2.0, 0.0))(
        rotate((0, 90, 0))(cylinder(r=r2, h=w, center=True, segments=24))
    )
    return arc + shuft


def link(w: float, r1: float, r2: float):
    return half_link(w, r1, r2) + rotate((0, 0, 180))(half_link(w, r1, r2))


N = 18
w = 3.0
r1 = 5.0
r2 = 0.9

elms = []
for i in range(N):
    dx = 15.0 * math.cos(2 * pi * i / N)
    dy = 15.0 * math.sin(2 * pi * i / N)
    elms.append(
        translate((dx, dy, 0))(
            rotate((0, 0, 90.0 + 360 / N * i))(
                rotate((90.0 * i, 0.0, 0.0))(link(w, r1, r2))
            )
        )
    )

chain = sum(elms[1:], elms[0])
scad_render_to_file(chain, filepath="chain.scad", include_orig_code=True)
# %%
