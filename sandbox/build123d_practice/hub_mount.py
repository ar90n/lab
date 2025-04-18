# %%
import math
from build123d import *
from ocp_vscode import show, show_object
import numpy as np
# %%
board_hole_hor_interval = 79.2
board_hole_ver_interval = 61.5
board_hole_rad = 5.4 / 2.0
board_hole_depth = 1.4
board_edge_rad = 8 / 2.0

wall_pillar_diameter = 5.4
wall_pillar_length = 5.1
wall_hole_interval = 25.2

mount_thickness = 3.0
# %%
board_hole_locs = np.array([
    (0.0, 0.0),
    (board_hole_hor_interval, board_hole_ver_interval),
])
wall_hole_locs = np.array([
    (0.0, 0.0),
    (3 * wall_hole_interval, 3 * wall_hole_interval),
])
# %%
with BuildPart() as board_pillar:
    spacer = Cylinder(board_hole_rad* 1.4, 3.3, align=[Align.CENTER, Align.CENTER, Align.MAX])
    pillar = Cylinder(board_hole_rad* 0.90, board_hole_depth + 3.3, align=[Align.CENTER, Align.CENTER, Align.MAX])
    pillar_top_face = pillar.faces().sort_by(Axis.Z)[0]
    with BuildSketch(pillar_top_face):
        Circle(board_hole_rad* 0.90 + 0.2)
    with BuildSketch(Plane(pillar_top_face).offset(3.0)):
        Circle(board_hole_rad* 0.90)
    loft()

    spacer_top_face = board_pillar.faces().sort_by(Axis.Z)[0]
    with BuildSketch(spacer_top_face):
        Rectangle(0.6, board_hole_rad* 2 + 1.5)
        Rectangle(board_hole_rad*2 + 1.5, 0.6)
    extrude(amount=-(3.0 + 0.6 * board_hole_depth), mode=Mode.SUBTRACT)
show(board_pillar.part)

# %%
with BuildPart() as wall_pillar:
    pillar = Cylinder(wall_pillar_diameter / 2.0 * 0.90, wall_pillar_length, align=[Align.CENTER, Align.CENTER, Align.MAX])
    pillar_top_face = pillar.faces().sort_by(Axis.Z)[0]
    with BuildSketch(pillar_top_face):
        Circle(wall_pillar_diameter / 2.0 * 0.90 + 0.2)
    with BuildSketch(Plane(pillar_top_face).offset(3.3)):
        Circle(wall_pillar_diameter / 2.0 * 0.90)
    loft()

    spacer_top_face = wall_pillar.faces().sort_by(Axis.Z)[0]
    with BuildSketch(spacer_top_face):
        Rectangle(0.95, wall_pillar_diameter + 1.5)
        Rectangle(wall_pillar_diameter + 1.5, 0.95)
    extrude(amount=-(wall_pillar_length * 0.6 + 3.3), mode=Mode.SUBTRACT)
show(wall_pillar.part)
# %%
with BuildPart() as board_wall_mount:
    with BuildSketch():
        with BuildLine():
            Spline([
                board_hole_locs[0],
                board_hole_locs[1],
            ])
            Spline([
                board_hole_locs[1],
                wall_hole_locs[1],
            ])
        trace(line_width=4.0)
        for loc in [*board_hole_locs, wall_hole_locs[1]]:
            with Locations(Location([float(l) for l in loc])):
                Circle(board_edge_rad)
    extrude(amount=mount_thickness)   

    for loc in board_hole_locs:
        with Locations(Location([float(l) for l in loc])):
            add(board_pillar)
    for loc in wall_hole_locs:
        with Locations(Location([*[float(l) for l in loc], mount_thickness])):
            add(wall_pillar, rotation=(0, 180,0))


# %%
show(board_wall_mount.part)
# %%
export_stl(board_wall_mount.part, "hub_mount.stl")
# %%
