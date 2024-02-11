# %%
import math
from build123d import *
from bd_warehouse.thread import (
    IsoThread,
)
from ocp_vscode import show, show_object
# IsoThread internal in the form of a nut
iso_internal = IsoThread(
    major_diameter=9.9 * MM,
    pitch=1 * MM,
    length=8.0 * MM,
    external=True,
    end_finishes=("chamfer", "fade"),
    hand="right",
)
# %%
neck_diameter = 15.0
neck_length = 35.0
base_width = 28.0
base_depth = 5
base_height = 2 * (((neck_diameter / 2.0) ** 2 + (neck_length / 2.0) ** 2) - (base_depth) ** 2) ** 0.5

# %%z
def gen():
    with BuildPart() as neck_shape:
        with BuildSketch():
            Circle(neck_diameter / 2.0)
        extrude(amount=neck_length)

    with BuildPart() as tapped:
        with BuildSketch():
            Circle(neck_diameter / 2.0)
            Circle(iso_internal.major_diameter / 2, mode=Mode.SUBTRACT)
        add(iso_internal.fuse(extrude(amount=iso_internal.length)))

    with BuildPart() as neck:
        with BuildSketch():
            Circle(neck_diameter / 2.0)
            Circle(iso_internal.major_diameter / 2, mode=Mode.SUBTRACT)
        extrude(amount=neck_length - iso_internal.length)
        with Locations(neck.part.faces().sort_by(Axis.Z).last.center_location):
            add(tapped)

    with BuildPart() as iso_internal_nut:
        base = Box(base_width, base_height, 2 * base_depth)

        base_topf = base.faces().sort_by(Axis.Z).last
        base_bottomf = base.faces().sort_by(Axis.Z).first
        with Locations(base_topf.center_location):
            with GridLocations(x_spacing=base_width - 2 * 4.0, y_spacing=base_height - 2 * 4.0, x_count=2, y_count=2):
                Hole(3.4 / 2.0)

        with BuildSketch(base_topf):
            with GridLocations(x_spacing=base_width - 2 * 4.0, y_spacing=base_height - 2 * 4.0, x_count=2, y_count=2):
                Circle(5.8 / 2.0)
        extrude(amount=-3.0, mode=Mode.SUBTRACT)

        with BuildSketch(base_bottomf):
            with GridLocations(x_spacing=base_width - 2 * 4.0, y_spacing=base_height - 2 * 4.0, x_count=2, y_count=2):
                RegularPolygon(radius=5.8 / 2.0, side_count=6)
        extrude(amount=-3.0, mode=Mode.SUBTRACT)

        with Locations((0.0, (neck_diameter / 1.0) / (2 ** 0.5), -(neck_diameter / 1.0) / (2 ** 0.5))):
            add(neck_shape, rotation=(45.0, .0, 0.0), mode=Mode.SUBTRACT)
            add(neck, rotation=(45.0, .0, 0.0))

        fillet(base.edges().filter_by(Axis.Z), radius=3.0)
        split(iso_internal_nut.part, Plane(base.faces().sort_by(Axis.Z).first), keep=Keep.BOTTOM)

    outside = split(iso_internal_nut.part, Plane.XY)
    inside = split(iso_internal_nut.part, Plane.XY, keep=Keep.BOTTOM)
    return outside, inside
# %%
outside_part, inside_part = gen()
show(outside_part)
outside_part.export_stl("fillament_gate_outside_part.stl")
# %%
show(inside_part)
inside_part.export_stl("fillament_gate_inside_part.stl")

# %%
