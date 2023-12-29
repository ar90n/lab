# %%
import math
from build123d import *
from bd_warehouse.thread import (
    IsoThread,
)
from ocp_vscode import show, show_object
# %%z
base_diameter = 32.0
base_depth = 6.0
neck_diameter = 15.0

# IsoThread internal in the form of a nut
iso_internal = IsoThread(
    major_diameter=9.9 * MM,
    pitch=1 * MM,
    length=8.0 * MM,
    external=True,
    end_finishes=("chamfer", "fade"),
    hand="right",
)
# %%z
is_outsize = False

def gen(is_outsize: bool):
    with BuildPart() as iso_internal_nut:
        base = Cylinder(base_diameter / 2, base_depth)

        base_topf = base.faces().sort_by(Axis.Z).last
        base_bottomf = base.faces().sort_by(Axis.Z).first
        with Locations(base_topf.center_location):
            Hole(iso_internal.major_diameter / 2)
            with PolarLocations((base_diameter + neck_diameter) / 4, 4, 0):
                Hole(3.4 / 2.0)

        f = base_topf if is_outsize else base_bottomf
        with BuildSketch(f):
            with PolarLocations((base_diameter + neck_diameter) / 4, 4, 0):
                if is_outsize:
                    Circle(5.8 / 2.0)
                else:
                     RegularPolygon(radius=5.8 / 2.0, side_count=6)
        extrude(amount=-3.0, mode=Mode.SUBTRACT)

        with BuildPart(base_topf):
            with BuildSketch():
                RegularPolygon(neck_diameter / 2.0, 6)
                Circle(iso_internal.major_diameter / 2, mode=Mode.SUBTRACT)
            neck = extrude(amount=iso_internal.length)
            add(iso_internal.fuse(neck))
    return iso_internal_nut

# %%
outside_part = gen(True)
show(outside_part)
outside_part.part.export_stl("outside_part.stl")
# %%
inside_part = gen(False)
show(inside_part)
inside_part.part.export_stl("inside_part.stl")
# %%
