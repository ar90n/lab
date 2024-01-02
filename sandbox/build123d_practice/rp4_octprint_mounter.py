# %%
import math
from build123d import *
from bd_warehouse.thread import (
    IsoThread,
)
from ocp_vscode import show, show_object
# %%
# %%
clearance = 0.5
camera_width = 28.0
camera_height = 30.0
camera_depth = 35.0
camera_mounter = 25.0
camera_hole_depth_interval = 18.0
camera_hole_hor_interval = 50.8 
pipe_diameter = 12.0 + 2.0 * clearance
rp4_height = 69.0
rp4_width = 94.0
rp4_depth = 26.0
body_margn = 5.0
body_depth = 6.0
base_height = pipe_diameter + 5.0
base_toe = 15.0 
base_depth = rp4_depth + 8.0 + base_toe + body_margn
body_width = rp4_width + 2.0 * body_margn
body_height = rp4_height + pipe_diameter + camera_mounter + 2.0 * body_margn
rp4_hole_hor_interval = (83.5 + 90.5) / 2.0
rp4_hole_ver_interval = (56.0 + 65.0) / 2.0
rp4_center_loc = (base_height + 2 * body_margn + rp4_height / 2.0, 0.0)
#rp4_center_loc = (0.0, 0.0)
# %%
with BuildPart() as mounter:
    with BuildSketch(Plane.XZ):
        with BuildLine():
            Polyline(*[(0.0, 0.0), (0.0, body_height), (body_depth + camera_depth, body_height), (body_depth + camera_depth, body_height - body_depth)])
            FilletPolyline(*[
                (body_depth + camera_depth, body_height - body_depth),
                (body_depth, body_height - body_depth),
                (body_depth, base_height), (base_depth - base_toe, base_height), (base_depth, base_height - base_toe), (base_depth, 0.0)], radius=5.0)
            Line(*[(base_depth, 0.0), (0.0, 0.0)])
        make_face()
        with Locations(((pipe_diameter / 2.0) + 2.0, 0.0)):
            Circle(pipe_diameter / 2.0, align=(Align.CENTER, Align.MIN), mode=Mode.SUBTRACT)
            Rectangle(pipe_diameter / 2.0, pipe_diameter / 2.0, align=(Align.CENTER, Align.CENTER), mode=Mode.SUBTRACT)
    extrude(amount = body_width)

    back_face = mounter.faces().sort_by(Axis.X).first
    rp4_work_center_loc = (-back_face.center_location.position.Z + rp4_center_loc[0], -back_face.center_location.position.X + rp4_center_loc[1])
    with BuildSketch(back_face):
        with Locations(rp4_work_center_loc):
            for cx, cy in [(0.5, 0.5), (0.5, -0.5), (-0.5, -0.5), (-0.5, 0.5)]:
                with Locations((cy * rp4_hole_ver_interval, cx * rp4_hole_hor_interval)):
                    RegularPolygon(radius=5.9 / 2.0, side_count=6)
    extrude(amount = -3.1, mode=Mode.SUBTRACT)

    with BuildSketch(back_face):
        with Locations(rp4_work_center_loc):
            for cx, cy in [(0.5, 0.5), (0.5, -0.5), (-0.5, -0.5), (-0.5, 0.5)]:
                r = 3.3 / 2.0
                loc = (cy * rp4_hole_ver_interval, cx * rp4_hole_hor_interval)
                with Locations(loc) as ll:
                    Circle(r)
                    with BuildLine(ll.local_locations[0]):
                        Polyline(*[(r * 2 ** -0.5, r * 2 ** -0.5), (0.0, r * 2 ** 0.5), (r * -2 ** -0.5, r * 2 ** -0.5), (r * 2 ** -0.5, r * 2 ** -0.5)])
                    make_face()
    extrude(amount = -body_depth, mode=Mode.SUBTRACT)

    camera_mounter_topf = mounter.faces().filter_by(Axis.Z).sort_by(Axis.Z).last
    with BuildSketch(camera_mounter_topf):
        locs = [
            (camera_hole_depth_interval / 2.0, camera_hole_hor_interval / 2.0),
            (-camera_hole_depth_interval / 2.0, camera_hole_hor_interval / 2.0),
            (-camera_hole_depth_interval / 2.0, -camera_hole_hor_interval / 2.0),
            (camera_hole_depth_interval / 2.0, -camera_hole_hor_interval / 2.0)
        ]
        with Locations(locs):
            Rectangle(1.2, 13.5)
    extrude(amount = -body_depth, mode=Mode.SUBTRACT)
show(mounter)
# %%
mounter.part.export_stl("rp4_octprint_mounter.stl")
# %%
