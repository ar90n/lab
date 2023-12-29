# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
lidar_hole_diameter = 3.0
lidar_center = (0.0, -4.0)
lidar_hole_locs = [
    ( 38.24 + lidar_center[0],  20.55 + lidar_center[1]),
    ( 28.40 + lidar_center[0], -43.80 + lidar_center[1]),
    (-27.00 + lidar_center[0], -47.10 + lidar_center[1]),
    (-34.00 + lidar_center[0],  23.75 + lidar_center[1])
]
pole_diameter = 9.5 
pole_height = 58.0
# %%
bw = (102.7 + 97.2) / 2.0
tw = (82.7 + 72.5) / 2.0
dl = (101.6+ 106.7) / 2.0
h = (dl **2 - ((bw + tw) / 2.0) ** 2) ** 0.5
base_hole_locs = [
    (tw / 2.0, h / 2.0),
    (bw / 2.0, -h / 2.0),
    (-bw / 2.0, -h / 2.0),
    (-tw / 2.0, h / 2.0)
]
# %%
board_height = (23.6 + 28.1) / 2.0
board_width = (68.0 + 63.4) / 2.0
board_center = (0.0, -7.0)
board_hole_locs = [
    ( board_width / 2.0 + board_center[0],  board_height / 2.0 + board_center[1]),
    ( board_width / 2.0 + board_center[0], -board_height / 2.0 + board_center[1]),
    (-board_width / 2.0 + board_center[0], -board_height / 2.0 + board_center[1]),
    (-board_width / 2.0 + board_center[0],  board_height / 2.0 + board_center[1])
]
# %%
with BuildPart() as mounter:
    with BuildLine():
        Spline(base_hole_locs[0], lidar_hole_locs[0], *board_hole_locs[:2], base_hole_locs[1])
        Spline(base_hole_locs[-1], lidar_hole_locs[-1], board_hole_locs[-1], board_hole_locs[-2], base_hole_locs[-2])
        Spline(lidar_hole_locs[1], board_hole_locs[1], board_hole_locs[2], lidar_hole_locs[2])
    trace(line_width=8.0)
    with BuildSketch():
        for loc in [*base_hole_locs, *board_hole_locs]:
            with Locations(loc):
                Circle(4.0)
        for loc in lidar_hole_locs:
            with Locations(loc):
                Circle(1.3 * pole_diameter / 2.0)
    extrude(amount = 5.3)
    btopz = mounter.faces().sort_by(Axis.Z).last.center_location.position.Z

    for loc in lidar_hole_locs:
        with Locations(Location(loc, (180, 0.0, 0))):
            CounterBoreHole(3.0 / 2.0, 5.7 / 2.0, 3.1)
    with BuildSketch(Plane((0.0, 0.0, btopz))):
        for loc in base_hole_locs:
            with Locations(loc):
                RegularPolygon(radius=5.7 / 2.0, side_count=6)
        for loc in board_hole_locs:
            with Locations(loc):
                RegularPolygon(radius=4.8 / 2.0, side_count=6)
    extrude(amount=-3.0, mode=Mode.SUBTRACT)

    for loc in base_hole_locs:
        with Locations((loc[0], loc[1], btopz)):
            Hole(3.3 / 2.0)
    for loc in board_hole_locs:
        with Locations(loc):
            Hole(2.5 / 2.0)
show(mounter)
# %%
with BuildPart() as pole:
    Cone(1.3 * pole_diameter / 2.0, pole_diameter / 2.0, pole_height)       
    topf = pole.faces().sort_by(Axis.Z).last
    bottomf = pole.faces().sort_by(Axis.Z).first
    with BuildSketch(topf):
        RegularPolygon(radius=5.7 / 2.0, side_count=6)
    extrude(amount=-3.0, mode=Mode.SUBTRACT)
    with BuildSketch(topf):
        Circle(radius=3.0 / 2.0)
    extrude(amount=-10.0, mode=Mode.SUBTRACT)

    with BuildSketch(bottomf):
        RegularPolygon(radius=5.7 / 2.0, side_count=6)
    extrude(amount=-3.0, mode=Mode.SUBTRACT)
    with BuildSketch(bottomf):
        Circle(radius=3.0 / 2.0)
    extrude(amount=-10.0, mode=Mode.SUBTRACT)
show(pole)
# %%
mounter.part.export_stl("lidar_mounter.stl")
pole.part.export_stl("lidar_pole.stl")
# %%
