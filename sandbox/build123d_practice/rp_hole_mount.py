# %%
import math
from build123d import *
from ocp_vscode import show
# %%
hole_width =10.0
hole_height = 2.5
hole_depth = 2.56 + 0.15

wall_hole_radius = 1.8
wall_depth = 6.5
# %%
with BuildPart() as hole_mount:
    with BuildSketch():
        RectangleRounded(hole_width, hole_height, hole_height / 2.0 - 1e-12)
    hole = extrude(amount=2.0)
    with BuildSketch(hole.faces().sort_by(Axis.Z).last):
        Circle(hole_height / 2.0)
    neck = extrude(amount=hole_depth)

    with BuildSketch(neck.faces().sort_by(Axis.Z).last):
        with Locations(Location(((0, wall_hole_radius - hole_height / 2.0)))):
            Circle(wall_hole_radius)
    pin = extrude(amount=wall_depth + wall_hole_radius)

    corner_loc = Location(pin.faces().sort_by(Axis.Z).last.location)
    with Locations(Location(Plane.ZX, corner_loc.position)):
        Sphere(wall_hole_radius)
        pc = Cylinder(wall_hole_radius, 3 * wall_depth / 4, rotation=(0, 45, 0), align=[Align.CENTER, Align.CENTER, Align.MIN])
    
    with Locations(pc.faces()[1]):
        Sphere(wall_hole_radius)
        Cylinder(wall_hole_radius, 3 * wall_depth / 4, rotation=(0, -45, 0), align=[Align.CENTER, Align.CENTER, Align.MIN])


# %%
show(hole_mount)
# %%
hole_mount.part.export_stl("hole_mount.stl")
# %%
