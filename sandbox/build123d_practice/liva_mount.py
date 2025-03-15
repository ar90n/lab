# %%
from build123d import *
from ocp_vscode import show, show_object

# %%
hole_center_ver_interval = 97.0
hole_center_hor_interval = 108.0
hole_rad = 4.0 / 2.0
spacer_depth = 1.4
body_offset = 0.7
hook_interval = 50.0
# %%
hole_locs = [
    (-hole_center_hor_interval/ 2.0, -hole_center_ver_interval / 2.0),
    (hole_center_hor_interval / 2.0, -hole_center_ver_interval / 2.0),
    (-hole_center_hor_interval / 2.0, hole_center_ver_interval / 2.0),
    (hole_center_hor_interval / 2.0, hole_center_ver_interval / 2.0),
]
with BuildPart() as part:
    with BuildSketch():
        for loc in hole_locs:
            with Locations(loc):
                Circle(8.0)
                Circle(hole_rad + 0.5, mode=Mode.SUBTRACT)
                Rectangle(2 * (hole_rad + 0.5), 8.0 * 2, mode=Mode.SUBTRACT, align=[Align.CENTER, Align.MIN])
    extrude(amount=spacer_depth)
    with BuildSketch(Plane.XY.offset(body_offset)):
        with BuildLine():
            Spline([(-6.0 -hole_center_hor_interval / 2.0, -hole_center_ver_interval / 2.0 - 7.0),(6.0 + hole_center_hor_interval / 2.0, -hole_center_ver_interval / 2.0 - 7.0)])
            Spline([(-6.0 -hole_center_hor_interval / 2.0, hole_center_ver_interval / 2.0 - 7.0),(6.0 + hole_center_hor_interval / 2.0, hole_center_ver_interval / 2.0 - 7.0)])
            Spline([(-hole_center_hor_interval / 2.0 + 6.0, -hole_center_ver_interval / 2.0 - 7.0),(hole_center_hor_interval / 2.0 - 6.0, hole_center_ver_interval / 2.0 - 7.0)])
            Spline([(-hole_center_hor_interval / 2.0 + 6.0, hole_center_ver_interval / 2.0 - 7.0),(hole_center_hor_interval / 2.0 - 6.0, -hole_center_ver_interval / 2.0 - 7.0)])
        trace(line_width=4.0)
        for loc in [(-hook_interval / 2.0, hole_center_ver_interval / 2.0 + 21.0), (hook_interval / 2.0, hole_center_ver_interval / 2.0 + 21.0)]:
            with Locations(loc):
                Circle(6)
                Rectangle(12, 26, align=[Align.CENTER, Align.MAX])
                Circle(2.5 + 0.3, mode=Mode.SUBTRACT)
    extrude(amount=3.0)

# %%
show(part.part)
# %%
export_stl(part.part, "liva_mount.stl")
# %%
