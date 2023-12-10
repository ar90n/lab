# %%
from build123d import *
from ocp_vscode import show
# %%
thickness = 2.5
margin = 0.3
inner_diameter =  72.0 + 2 * margin
outer_diameter =  inner_diameter + thickness
bottom_thickness = 4.0
# %%
with BuildPart() as p:
    with BuildSketch():
        Circle(outer_diameter / 2.0)
    zz = extrude(amount=bottom_thickness + 5.0)
    with BuildSketch(zz.faces().sort_by(Axis.Z).last):
        Circle(inner_diameter / 2.0)
    extrude(amount=-5.0, mode=Mode.SUBTRACT)
show(p)
# %%
p.part.export_stl("cover.stl")
# %%
