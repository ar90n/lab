# %%
import math
from build123d import *
from ocp_vscode import show, show_object
import numpy as np
# %%
lcd_hole_hor_far_interval = 116.6
lcd_hole_hor_near_interval = 109.6
lcd_hole_ver_far_interval = 89.7
lcd_hole_ver_near_interval = 82.7
lcd_hole_hor_interval = (lcd_hole_hor_far_interval + lcd_hole_hor_near_interval) / 2.0
lcd_hole_ver_interval = (lcd_hole_ver_far_interval + lcd_hole_ver_near_interval) / 2.0
lcd_hole_hor_rad = (lcd_hole_hor_far_interval - lcd_hole_hor_near_interval) / 4.0
lcd_hole_ver_rad = (lcd_hole_ver_far_interval - lcd_hole_ver_near_interval) / 4.0
lcd_hole_rad = ((lcd_hole_hor_rad + lcd_hole_ver_rad) / 2.0) * 0.9
lcd_hole_depth = 1.8
lcd_edge_rad = 3.8

wall_pillar_diameter = 5.4
pillar_length = 5.1
wall_hole_interval = 148.0

wall_pillar_offset = (25.4 + wall_pillar_diameter) / 2.0

mount_thickness = 3.0
# %%
lcd_hole_locs = np.array([
    (-lcd_hole_hor_interval / 2.0, -lcd_hole_ver_interval / 2.0),
    (lcd_hole_hor_interval / 2.0, -lcd_hole_ver_interval / 2.0),
    (-lcd_hole_hor_interval / 2.0, lcd_hole_ver_interval / 2.0),
    (lcd_hole_hor_interval / 2.0, lcd_hole_ver_interval / 2.0),
])
lcd_hole_inner_locs = 0.5 * lcd_hole_locs
wall_hole_locs = [
    (-wall_hole_interval / 2.0, -lcd_hole_ver_interval / 2.0 + wall_pillar_offset),
    (wall_hole_interval / 2.0, -lcd_hole_ver_interval / 2.0 + wall_pillar_offset),
]
# %%
with BuildPart() as lcd_pillar:
    spacer = Cylinder(lcd_hole_rad* 1.4, 2.0 , align=[Align.CENTER, Align.CENTER, Align.MAX])
    pillar = Cylinder(lcd_hole_rad* 0.90, lcd_hole_depth + 2.0, align=[Align.CENTER, Align.CENTER, Align.MAX])
    pillar_top_face = pillar.faces().sort_by(Axis.Z)[0]
    with BuildSketch(pillar_top_face):
        Circle(lcd_hole_rad* 0.90 + 0.2)
    with BuildSketch(Plane(pillar_top_face).offset(3.0)):
        Circle(lcd_hole_rad* 0.90)
    loft()

    spacer_top_face = lcd_pillar.faces().sort_by(Axis.Z)[0]
    with BuildSketch(spacer_top_face):
        Rectangle(0.80, lcd_hole_rad* 2 + 1.5)
        Rectangle(lcd_hole_rad*2 + 1.5, 0.8)
    extrude(amount=-(3.0 + 0.6 * lcd_hole_depth), mode=Mode.SUBTRACT)
show(lcd_pillar.part)
# %%
with BuildPart() as lcd_wall_mount:
    with BuildSketch():
        with BuildLine():
            Spline([
                lcd_hole_locs[0],
                lcd_hole_inner_locs[0],
                lcd_hole_inner_locs[1],
                lcd_hole_locs[1],
            ])
            Spline([
                lcd_hole_inner_locs[0],
                lcd_hole_inner_locs[2],
            ])
            Spline([
                lcd_hole_inner_locs[1],
                lcd_hole_inner_locs[3],
            ])
            Spline([
                lcd_hole_locs[2],
                lcd_hole_inner_locs[2],
                lcd_hole_inner_locs[3],
                lcd_hole_locs[3],
            ])
            Spline([
                lcd_hole_locs[0],
                wall_hole_locs[0],
                lcd_hole_inner_locs[2],
            ])
            Spline([
                lcd_hole_locs[1],
                wall_hole_locs[1],
                lcd_hole_inner_locs[3],
            ])
        trace(line_width=4.0)
        for loc in lcd_hole_locs:
            with Locations(Location([float(l) for l in loc])):
                Circle(lcd_edge_rad)
        for loc in wall_hole_locs:
            with Locations(Location([float(l) for l in loc])):
                Circle(wall_pillar_diameter / 2.0 + 2.0)
                Circle(wall_pillar_diameter / 2.0 + 0.10, mode=Mode.SUBTRACT)
    extrude(amount=mount_thickness)   

    for loc in lcd_hole_locs:
        with Locations(Location([float(l) for l in loc])):
            add(lcd_pillar)


# %%
show(lcd_wall_mount.part)
# %%
export_stl(lcd_wall_mount.part, "lcd_wall_mount.stl")