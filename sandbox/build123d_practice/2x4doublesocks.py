# %%
from build123d import *
from ocp_vscode import show
# %%
w = 89.0
h = 38.0
d = 60.0
margin = 0.05
thickness = 3.0
brim_size = 30.0
inner_w =  2 * w + 2 * margin
inner_h =  h + 2 * margin
outer_w = inner_w + 2 * thickness
outer_h = inner_h + 2 * thickness
base_w = outer_w + 2 * brim_size
base_h = outer_h + 2 * brim_size
bottom_thickness = 5.0

# %%
with BuildPart() as p:
    with BuildSketch():
        RectangleRounded(base_w, base_h, 10)
        with GridLocations(x_spacing=(base_w / 2 - 2 * 4), y_spacing=(base_h - 2 * 8), x_count=3, y_count=2):
            Circle(2.5, mode=Mode.SUBTRACT)
    zz = extrude(amount=bottom_thickness)

    with Locations((0, 0, bottom_thickness)):
        with GridLocations(x_spacing=(base_w / 2 - 2 * 4), y_spacing=(base_h - 2 * 8), x_count=3, y_count=2):
            Cone(5, 2.5, 4.0, rotation=(180, 0, 0), mode=Mode.SUBTRACT)

    with BuildSketch(zz.faces().sort_by(Axis.Z).last):
        Rectangle(outer_w, outer_h)
    bb = extrude(amount=d - bottom_thickness)
    chamfer(bb.edges().sort_by(Axis.Z).group_by(Axis.Z)[0], 15, 20)

    with BuildSketch(bb.faces().sort_by(Axis.Z).last):
        Rectangle(inner_w, inner_h)
    bb = extrude(amount=bottom_thickness - d, mode=Mode.SUBTRACT)
show(p)
# %%
p.part.export_stl("2x4socks.stl")
# %%
