# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
margin = 1.0
depth = 5.0
mount_hole_interval = 85.0
hor_hole_interval = 58.0
ver_hole_interval = 49.0
edge_to_hole_center = 3.5 + margin
board_width = mount_hole_interval + 2.0 * edge_to_hole_center
board_height = ver_hole_interval + 2.0 * edge_to_hole_center
hole_diameter = 3.0

# %%
hole_locs = [
    (edge_to_hole_center, edge_to_hole_center),
    (edge_to_hole_center, edge_to_hole_center + ver_hole_interval),
    (edge_to_hole_center + hor_hole_interval, edge_to_hole_center + ver_hole_interval),
    (edge_to_hole_center + hor_hole_interval, edge_to_hole_center),
    (board_width - edge_to_hole_center, board_height - edge_to_hole_center),
    (board_width - edge_to_hole_center, edge_to_hole_center),
]
with BuildPart() as rems:
    Box(board_width, board_height, depth / 2.0, align=[Align.MIN, Align.MIN, Align.MAX])
    bottomz = rems.faces().sort_by(Axis.Z).first.center_location.position.Z
    topf = rems.faces().sort_by(Axis.Z).last
    for i, loc in enumerate(hole_locs):
        with Locations((*loc, bottomz)):
            ppp = Cylinder(1.42 * edge_to_hole_center, depth, mode=Mode.SUBTRACT)
            if 3 < i:
                with BuildSketch(ppp.faces().sort_by(Axis.Z).last):
                    RegularPolygon(radius=3.1, side_count=6)
                extrude(amount=-4.4)

with BuildPart() as mounter:
    Box(board_width, board_height, depth, align=[Align.MIN, Align.MIN, Align.CENTER])
    es = mounter.edges().filter_by(Axis.Z)
    bottomz = mounter.faces().sort_by(Axis.Z).first.center_location.position.Z
    topz = mounter.faces().sort_by(Axis.Z).last.center_location.position.Z
    for i, loc in enumerate(hole_locs):
        with Locations((*loc, bottomz)):
            if i < 4:
                Cylinder(hole_diameter / 2.0 + 1.0, 2.5)

        with Locations((*loc, topz)):
            if i in (2, 3):
                CounterBoreHole(hole_diameter / 2.0, 5.7 / 2.0, 3.5)
            else:
                Hole(hole_diameter / 2.0)

    add(rems, mode=Mode.SUBTRACT)
    fillet(es, radius=edge_to_hole_center)
show(mounter)
# %%
mounter.part.export_stl("rp4_mounter.stl")

# %%
