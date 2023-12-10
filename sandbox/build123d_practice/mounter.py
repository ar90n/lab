# %%
from build123d import *
from ocp_vscode import show
# %%
base_hor_hole_interval = 38.0
base_ver_hole_interval = 88.0
board_hor_hole_interval = 64.8
board_ver_hole_interval = 82.8
margin = 1.5 + 3.0
# %%
hor_diff = (board_hor_hole_interval - base_hor_hole_interval) / 2.0
ver_diff = (base_ver_hole_interval - board_ver_hole_interval) / 2.0
base_hor_offset_to_center = base_hor_hole_interval / 2.0
board_hor_offset_to_center = board_hor_hole_interval / 2.0

# %%
mounter_width = 2 * margin + board_hor_hole_interval
mounter_height = 2 * margin + ver_diff
mounter_depth = 6.0

# %%
with BuildPart() as p:
    with BuildSketch() as s:
        Rectangle(mounter_width / 2.0, mounter_height, align=[Align.MIN, Align.CENTER, Align.CENTER])
        with Locations((board_hor_offset_to_center, -ver_diff / 2.0)):
            Circle(1.8, mode=Mode.SUBTRACT)
    zz = extrude(amount=mounter_depth)
    fillet(p.faces().sort_by(Axis.X).last.edges().filter_by(Axis.Z), 2.0)

    with Locations((0, 0, zz.faces().sort_by(Axis.Z).last.center().Z)):
        with Locations((base_hor_offset_to_center, ver_diff / 2.0)):
            CounterBoreHole(1.8, 3.1, 2.0)
    with BuildSketch():
        with Locations((board_hor_offset_to_center, -ver_diff / 2.0)):
            RegularPolygon(radius=3.1, side_count=6)
    extrude(amount=4.2, mode=Mode.SUBTRACT)
    mirror(about=Plane.YZ)

show(p)
# %%
p.part.export_stl("mounter.stl")
# %%
