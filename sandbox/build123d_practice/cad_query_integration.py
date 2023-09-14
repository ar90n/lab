# %%
from typing import Any
from build123d import *
from cq_gears import SpurGear

# %%
spur_gear = SpurGear(
    module=5.0, teeth_number=14, width=2.50, clearance=0.0, backlash=0.25, bore_d=5
)


# %%
def to_build123d(obj):
    gear = Solid.make_box(1, 1, 1)
    gear.wrapped = obj.build().wrapped
    return gear


# %%
a, b = 2**0.5 * spur_gear.r0, 4

gear_locs = PolarLocations(radius=a, count=b, rotate=False)
plate = Cylinder(radius=100, height=2.0)
plane = Plane(plate.faces().sort_by(Axis.Z).last)
plate += plane.offset(1.0) * gear_locs * Cylinder(radius=10, height=2.0)
plate += plane.offset(1.0 + 2.5) * gear_locs * Cylinder(radius=2.3, height=5.0)
gears = plane.offset(2.0 + 0.5) * gear_locs * to_build123d(spur_gear)
# %%
gears_assembly = Compound(label="assembly", children=[plate, *gears])
gears_assembly