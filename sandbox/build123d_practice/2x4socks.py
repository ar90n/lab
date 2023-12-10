# %%
from build123d import *
from ocp_vscode import show
# %%
w = 89.0
h = 38.0
d = 40.0
margin = -0.4
thickness = 3.0
inner_w =  w + 2 * margin
inner_h =  h + 2 * margin
outer_w = inner_w + 2 * thickness
outer_h = inner_h + 2 * thickness
bottom_thickness = 5.0
# %%
with BuildPart() as p:
    with BuildSketch():
        Rectangle(outer_w, outer_h)
    zz = extrude(amount=bottom_thickness)
    with BuildSketch(zz.faces().sort_by(Axis.Z).last):
        Rectangle(outer_w, outer_h)
        Rectangle(inner_w, inner_h, mode=Mode.SUBTRACT)
    extrude(amount=d - bottom_thickness)
show(p)
# %%
p.part.export_stl("2x4socks.stl")
# %%
