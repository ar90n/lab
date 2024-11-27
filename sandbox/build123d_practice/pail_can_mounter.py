# %%
import math
from build123d import *
from ocp_vscode import show

# %%
bottom_radius = (830.0 / (2 * math.pi))
base_width = 30.0
base_height = 5.0
wall_width = 7.5
wall_height = 40.0

# %%
pts = [
    (-bottom_radius, 0),
    (-bottom_radius - base_width, 0),
    (-bottom_radius - base_width, base_height),
    (-bottom_radius - wall_width, base_height),
    (-bottom_radius - wall_width, base_height + wall_height),
    (-bottom_radius, base_height + wall_height),
]
# %%
with BuildPart() as p:
    with BuildSketch(Plane.XZ) as sk:
        with BuildLine():
            l1 = Polyline(*pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
    revolve(axis=Axis.Z, revolution_arc=60.0)
    fillet(p.faces().sort_by(Axis.Z)[2].edges().filter_by(GeomType.CIRCLE).sort_by(SortBy.RADIUS)[0], 20.0)
    chamfer(p.faces().sort_by(Axis.Z)[-1].edges().filter_by(GeomType.CIRCLE).sort_by(SortBy.RADIUS)[0], 5.0)

    with Locations((0, 0, base_height)):
        with PolarLocations(radius=(bottom_radius + base_width - 10.0), start_angle=15.0, count=12):
            Cylinder(2.5, wall_height, mode=Mode.SUBTRACT)
            Cylinder(5, wall_height, align=(Align.CENTER, Align.CENTER, Align.MIN),  mode=Mode.SUBTRACT)
            Cone(5, 2.5, 4.0, rotation=(180, 0, 0), mode=Mode.SUBTRACT)
# %%
show(p)

# %%
p.part.export_stl("pail_can_mounter.stl")

# %%
