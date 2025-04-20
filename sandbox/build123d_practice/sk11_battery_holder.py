# %%
import math
from build123d import *
from ocp_vscode import show
# %%
wall_thickness = 1.5
dist_from_center = 21.0
d2 = 33 + wall_thickness
d3 = d2 - 7.2
a = d2 * math.cos(math.pi / 6) * 2
rad = (math.sqrt(3)  - 1) / 4 * a
height = 31
# %%
corners = [
    (0, d2),
    (-d2 * math.cos(math.pi / 6), -d2 * math.sin(math.pi/6)),
    (d2 * math.cos(math.pi / 6), -d2 * math.sin(math.pi/6)),
]

holes = [
    (0, d3),
    (-d3 * math.cos(math.pi / 6), -d3 * math.sin(math.pi/6)),
    (d3 * math.cos(math.pi / 6), -d3 * math.sin(math.pi/6)),
]

# %%
with BuildPart() as holder_body:
    with BuildSketch():
        with BuildLine():
            Polyline([
                *corners
            ], close=True)
        make_face()
    extrude(amount=height)
    body_vedges = holder_body.edges().filter_by(Axis.Z)
    fillet(body_vedges, radius=rad)

with BuildPart() as holder_mount:
    with BuildSketch():
        with BuildLine():
            Polyline([
                *corners
            ], close=True)
        make_face()
        with Locations(holes):
            Circle(3.8 / 2.0, mode=Mode.SUBTRACT)

    extrude(amount=2.0)
    mount_vedges = holder_mount.edges().filter_by(Axis.Z)
    fillet(mount_vedges, radius=2.0)

    add(holder_body.part, mode=Mode.SUBTRACT)

with BuildPart() as holder_wall:
    add(holder_body.part)
    offset(amount=-wall_thickness, openings=holder_wall.faces().filter_by(Axis.Z)[1])

with BuildPart() as holder:
    add(holder_wall.part)
    add(holder_mount.part)
    with Locations(Plane.XY.offset(wall_thickness)):
        Box(a, a / 3 + 0.8, height -wall_thickness, align=(Align.CENTER, Align.MIN, Align.MIN), mode=Mode.SUBTRACT)



# %%
show(holder)
# %%
export_stl(holder.part, "sk11_battery_holder.stl")
# %%
