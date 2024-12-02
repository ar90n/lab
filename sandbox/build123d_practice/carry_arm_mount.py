# %%
import math
from build123d import *
from ocp_vscode import show
# %%
base_width = 140.0
base_height = 40.0
base_length = 5.0

# %%
with BuildPart() as p:
    with BuildSketch():
        Rectangle(base_width / 2, base_height / 3, align=(Align.MIN, Align.CENTER))
        with Locations((base_width / 2 + 2.5, 0, 0)):
            Rectangle(15, base_height, align=(Align.MAX, Align.CENTER))
    bb = extrude(amount=base_length)

    with GridLocations(x_spacing=base_width /2 , y_spacing=(base_height - 10), x_count=1, y_count=2):
        with Locations((base_width / 2 - 5, 0, base_length)):
            Cylinder(2.5, 10.0, mode=Mode.SUBTRACT)
            Cone(5, 2.5, 4.0, rotation=(180, 0, 0), mode=Mode.SUBTRACT)
    with BuildSketch(Location((0, 0, base_length))):
        with Locations((125 / 2.0, 0, 0)):
            Circle(13.0 / 2.0)
            Circle(10.5 / 2.0, mode=Mode.SUBTRACT)
    extrude(amount=10.0)

    with BuildSketch():
        with Locations((125 / 2.0, 0, 0)):
            Circle(5 / 2.0)
    extrude(amount=10.0, mode=Mode.SUBTRACT)

    with BuildSketch():
        with Locations((125 / 2.0, 0, 0)):
            Circle(10.0 / 2.0)
    extrude(amount=4.0, mode=Mode.SUBTRACT)
    mirror(about=Plane.YZ)

    fillet(p.edges().filter_by(Axis.Z), 6)
    fillet(p.edges().filter_by(GeomType.CIRCLE).group_by(Axis.Z)[3].sort_by(SortBy.RADIUS)[-2:], 2.5)
    fillet(p.edges().filter_by(GeomType.CIRCLE,reverse=True).group_by(Axis.Z)[6], 1.5)
    chamfer(p.edges().filter_by(GeomType.CIRCLE).group_by(Axis.Z)[-1].sort_by(SortBy.RADIUS)[-2:], 1.0)

# %%
show(p)
# %%
p.part.export_stl("carry_arm_mount.stl")

# %%
