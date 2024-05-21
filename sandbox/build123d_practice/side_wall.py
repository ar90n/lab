# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
pipe_diameter = 8.0
pipe_clearance = 0.25
pipe_interval = 60.0 * 2 ** 0.5
pipe_height = 40.0
pipe_location = (pipe_interval / 2.0, pipe_height)
pipe_region_radius = 15.0
# %%
wall_height = 50.0
wall_width =  110.0
wall_depth = 15.0
base_height = 5.0
base_max_margin = 5.0 * 2
base_min_margin = 1.2 * 2
base_slope_ratio = 0.5

# %%
with BuildPart() as wall:
    with BuildSketch(Plane.XY):
        with BuildLine():
            Polyline([
                (0.0, 0.0),
                (wall_width / 2.0, 0.0),
                (wall_width / 2.0, wall_height),
                (wall_height / 2.0, wall_height),
                (1 * wall_height / 4.0, 3 * wall_height / 4.0),
                (0.0,  3 * wall_height / 4.0),
                (0.0, 0.0)
            ])
        make_face()
    extrude(amount=wall_depth / 2.0, both=True)

    front_f = wall.faces().sort_by(Axis.Z).first
    with BuildSketch(Location((*pipe_location, front_f.center_location.position.Z + (wall_depth - 5.0)))):
        Circle((pipe_diameter + 2 * pipe_clearance) / 2.0)
    with BuildSketch(Location((*pipe_location, front_f.center_location.position.Z))):
        Circle((pipe_diameter + pipe_clearance ) / 2.0)
    loft(mode=Mode.SUBTRACT)

    e = [
        wall.faces().sort_by(Axis.Y).last.edges().filter_by(Axis.Z),
        wall.faces().sort_by(Axis.Y)[-2].edges().filter_by(Axis.Z)
    ]
    fillet(e, radius=10.0)

    bottom_f = wall.faces().sort_by(Axis.Y).first

with BuildPart() as cwall:
    with BuildSketch(Plane.XY):
        with BuildLine():
            Polyline([
                (0.0, 0.0),
                (wall_width / 2.0, 0.0),
                (wall_width / 2.0, wall_height),
                (wall_height / 2.0, wall_height),
                (1 * wall_height / 4.0, 3 * wall_height / 4.0),
                (0.0,  3 * wall_height / 4.0),
                (0.0, 0.0)
            ])
        make_face()
    extrude(amount=wall_depth / 2.0, both=True)

    front_f = cwall.faces().sort_by(Axis.Z).first
    with BuildSketch(Location((*pipe_location, front_f.center_location.position.Z + ((wall_depth - 2.5) / 2.0)))):
        Circle((pipe_diameter + 2 * pipe_clearance) / 2.0)
    with BuildSketch(Location((*pipe_location, front_f.center_location.position.Z))):
        Circle((pipe_diameter + pipe_clearance ) / 2.0)
    loft(mode=Mode.SUBTRACT)

    bottom_f = cwall.faces().sort_by(Axis.Z).last
    with BuildSketch(Location((*pipe_location, bottom_f.center_location.position.Z + ((wall_depth - 2.5) / 2.0)))):
        Circle((pipe_diameter + 2 * pipe_clearance) / 2.0)
    with BuildSketch(Location((*pipe_location, bottom_f.center_location.position.Z))):
        Circle((pipe_diameter + pipe_clearance ) / 2.0)
    loft(mode=Mode.SUBTRACT)

    e = [
        cwall.faces().sort_by(Axis.Y).last.edges().filter_by(Axis.Z),
        cwall.faces().sort_by(Axis.Y)[-2].edges().filter_by(Axis.Z)
    ]
    fillet(e, radius=10.0)

    bottom_f = cwall.faces().sort_by(Axis.Y).first

with BuildPart() as base:
    bottom_f = wall.faces().sort_by(Axis.Y).first

    with BuildSketch(Plane.XZ):
        Rectangle((wall_width + base_max_margin) / 2.0, wall_depth + base_max_margin, align=[Align.MIN, Align.CENTER])
    extrude(amount = base_height)
    with BuildSketch(Plane.XZ):
        Rectangle((wall_width + base_max_margin) / 2.0, wall_depth + base_max_margin, align=[Align.MIN, Align.CENTER])
    l = (base_max_margin - base_min_margin) / (2 * base_slope_ratio)
    with BuildSketch(Plane.XZ.offset(-l)):
        Rectangle((wall_width + base_min_margin) / 2.0, wall_depth + base_min_margin, align=[Align.MIN, Align.CENTER])
    loft()
    add(wall, mode=Mode.SUBTRACT)


with BuildPart() as wall2:
    add(wall)
    bottom_f = wall.faces().sort_by(Axis.Y).first

    with BuildSketch(bottom_f):
        RegularPolygon(radius=5.8 / 2.0, side_count=6, rotation=30.0)
    extrude(amount=-3.0, mode=Mode.SUBTRACT)
    with BuildSketch(bottom_f):
        RegularPolygon(radius=3.7 / 2.0, side_count=6, rotation=30.0)
    extrude(amount=-10.0, mode=Mode.SUBTRACT)
    mirror(wall2.part, Plane.ZY)

with BuildPart() as cwall2:
    add(cwall)
    bottom_f = cwall.faces().sort_by(Axis.Y).first

    with BuildSketch(bottom_f):
        RegularPolygon(radius=5.8 / 2.0, side_count=6, rotation=30.0)
    extrude(amount=-3.0, mode=Mode.SUBTRACT)
    with BuildSketch(bottom_f):
        RegularPolygon(radius=3.7 / 2.0, side_count=6, rotation=30.0)
    extrude(amount=-10.0, mode=Mode.SUBTRACT)
    mirror(cwall2.part, Plane.ZY)

with BuildPart() as base2:
    add(base)
    bottom_f = wall.faces().sort_by(Axis.Y).first
    with BuildSketch(bottom_f):
        Circle(radius=3.0 / 2.0)
    extrude(amount=5.0, mode=Mode.SUBTRACT)

    bz = wall.faces().sort_by(Axis.Y).first.center_location.position.Z
    with BuildSketch(Plane(bottom_f).offset(base_height)):
        Circle(radius=6.0 / 2.0)
    extrude(amount=-3.0, mode=Mode.SUBTRACT)
    mirror(base2.part, Plane.ZY)

# %%
show(wall2)
wall2.part.export_stl("wall2.stl")
# %%
show(cwall2)
cwall2.part.export_stl("cwall2.stl")
# %%
show(base2)
base2.part.export_stl("base2.stl")
# %%
