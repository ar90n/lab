# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
wall_thickness = 1.2
cover_width = 160.0 + 2 * wall_thickness
cover_depth = 38.2 + 5.0 + wall_thickness
cover_front_width= 46.3 + 2 * wall_thickness
cover_front_height = 60.8
cover_wheel_height = (64.0 + 69.3) / 2.0
cover_wheel_rad = 50.0 / 2.0 + wall_thickness

inner_width = 0.75
collector_inner_diameter = 38.0 - 2.0 * inner_width
collector_outer_diameter = 40.0
outer_height = 30.0
# %%
with BuildPart() as cover:
    with BuildSketch() as sk:
        with BuildLine():
            l1 = Polyline([
                (0.0, 0.0),
                (cover_width / 2.0, 0.0),
                (cover_width / 2.0, cover_wheel_height)
            ])
            l2 = ThreePointArc([
                (cover_width / 2.0, cover_wheel_height),
                (cover_width / 2.0 - cover_wheel_rad, cover_wheel_height + cover_wheel_rad),
                (cover_width / 2.0 - 2 * cover_wheel_rad, cover_wheel_height + 2.6)
            ])
            Polyline([
                l2 @ 1,
                (-cover_width / 2.0 + cover_front_width, cover_front_height - 8.8),
                (-cover_width / 2.0 + cover_front_width, cover_front_height),
                (-cover_width / 2.0, cover_front_height),
                (-cover_width / 2.0, 0.0),
                (0.0, 0.0)
            ])
        make_face()
    extrude(amount=cover_depth)

    fillet(cover.edges().filter_by(Axis.Z), radius=1.1)
    fillet(cover.faces().sort_by(Axis.Z)[-1].edges(), radius=1.1)

    topf = cover.faces().sort_by(Axis.Z)[0]
    offset(cover.solids()[0], amount=-1.0, openings=topf)

    f = faces().sort_by(Axis.Y)[-10]
    with BuildSketch(f):
        Rectangle(1.23 * (cover_width - (cover_front_width + 2 * cover_wheel_rad)), cover_depth - 0.30)
    extrude(amount=4.5, mode=Mode.SUBTRACT)

    with BuildSketch(f):
        Rectangle(1.23 * (cover_width - (cover_front_width + 2 * cover_wheel_rad)), cover_depth - 0.30, align=[Align.CENTER, Align.MAX])
    extrude(amount=-4.5, mode=Mode.SUBTRACT)

    ff = faces().sort_by(Axis.X).first
    with BuildSketch(ff):
        Circle(collector_outer_diameter / 2.0)
    tt = extrude(amount=outer_height)
    with BuildSketch(tt.faces().sort_by(Axis.X)[0]):
        Circle(collector_inner_diameter / 2.0)
    extrude(amount=-(outer_height + wall_thickness), mode=Mode.SUBTRACT)


# %%
show(cover.part)
# %%
export_stl(cover.part, "sander_cover.stl")
# %%
