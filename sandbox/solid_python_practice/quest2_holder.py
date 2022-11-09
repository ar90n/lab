# %%
from solid import *
from solid.utils import *

hole_interval = 107.7
phi1 = 5.5
phi2 = 8.5
depth1 = 3.0
depth2 = 2.0


def pochi():
    margin = 1.0
    r1 = (phi1 - margin) / 2.0
    c1 = cylinder(r=r1, h=depth1, segments=32, center=False)
    r2 = (phi2 - margin) / 2.0
    c2 = cylinder(r=r2, h=depth2, segments=32, center=False)
    return c1 + translate((0, 0, depth1 ))(c2)


def hikkake():
    return cube(size=[9.0, 11.5, 2.3], center=False) - translate([-0.1, 7.9, -0.1])(cube(size=[3.2, 3.8, 2.5], center=False))

body_depth = 5.0
body_height = 24.8
body_width = 140.0
half_body = translate([0.0, -body_height / 2.0, 0.0])(cube(size=[body_width / 2.0, body_height, body_depth], center=False))
half_body += translate([(hole_interval + phi2) / 2.0, 0.0, body_depth])(pochi())
body = mirror([1.0, 0.0, 0.0])(half_body) + half_body
body += translate([-12.7, 0.0, 0.0])(cube([25.4, 35.4, body_depth], center=False))

holder = body + translate([0.05, -body_height/2.0, 0.0])(rotate(90.0, [0.0, 1.0, 0.0])(translate([0.0, 25.4, 0.0])(hikkake()) + hikkake()))
scad_render_to_file(holder, filepath="holder.scad", include_orig_code=True)
# %%
