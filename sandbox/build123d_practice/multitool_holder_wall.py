# %%
import math
from build123d import *
from ocp_vscode import show

# %%
width = 13 * CM
height = 18 * CM
wall_thickness = 3.5 * MM
# %%
with BuildPart() as body:
    with BuildSketch():
        Rectangle(width, height)
    extrude(amount=wall_thickness)

    top_face = body.faces().sort_by(Axis.Z)[-1]
    with Locations((-width /2.0 + 6.0, height /2.0 - 6.0, top_face.center().Z)):
        CounterSinkHole(3.8 / 2.0, 8.0 / 2.0, wall_thickness, mode=Mode.SUBTRACT)
    with Locations((width /2.0 - 6.0, height /2.0 - 6.0, top_face.center().Z)):
        CounterSinkHole(3.8 / 2.0, 8.0 / 2.0, wall_thickness, mode=Mode.SUBTRACT)
    with Locations((width /2.0 - 6.0, height /2.0 - 90.0 + 6.0, top_face.center().Z)):
        CounterSinkHole(3.8 / 2.0, 8.0 / 2.0, wall_thickness, mode=Mode.SUBTRACT)

    with Locations(Plane.XY.offset(wall_thickness)):
        with GridLocations(width/ 5, height / 6, x_count=4, y_count=6):
            Cylinder(5.0 + 0.2, 2.1, mode=Mode.SUBTRACT, align=(Align.CENTER, Align.CENTER, Align.MAX))

# %%
R = 100.0
wall_thickness2 = 1.5 * MM
pocket_height = 14
centers = [
    [-R/2, -math.sqrt(3)/2*R * 1 / 3],              
    [R/2, -math.sqrt(3)/2*R * 1 / 3],                
    [0, math.sqrt(3)/2*R * 2 / 3]    
]
# %%
with BuildPart() as pocket:
    with BuildSketch():
        for i, center in enumerate(centers):
            with Locations(Location(center)):
                Circle(R, mode=Mode.ADD if i == 0 else Mode.INTERSECT)
    extrude(amount=wall_thickness2 + pocket_height)

    open_face = [
        pocket.faces().sort_by(Axis.Y)[0],
        pocket.faces().sort_by(Axis.Z)[0]
    ]
    offset(amount=-wall_thickness2, openings=open_face)

with BuildPart() as pocket2:
    with BuildSketch():
        for i, center in enumerate(centers):
            with Locations(Location(center)):
                Circle(R + 0.25, mode=Mode.ADD if i == 0 else Mode.INTERSECT)
    extrude(amount=wall_thickness2 + pocket_height)

    open_face = [
        pocket2.faces().sort_by(Axis.Y)[0],
        pocket2.faces().sort_by(Axis.Z)[0]
    ]
    offset(amount=-(wall_thickness2 + 0.5), openings=open_face)

show(pocket)
# %%
with BuildPart() as body_left:
    with BuildSketch():
        Rectangle(width, height)
    extrude(amount=wall_thickness)

    top_face = body_left.faces().sort_by(Axis.Z)[-1]
    with Locations((-width /2.0 + 6.0, height /2.0 - 6.0, top_face.center().Z)):
        CounterSinkHole(3.8 / 2.0, 8.0 / 2.0, wall_thickness, mode=Mode.SUBTRACT)
    with Locations((width /2.0 - 6.0, height /2.0 - 6.0, top_face.center().Z)):
        CounterSinkHole(3.8 / 2.0, 8.0 / 2.0, wall_thickness, mode=Mode.SUBTRACT)
    with Locations((-width /2.0 + 6.0, height /2.0 - 90.0 + 6.0, top_face.center().Z)):
        CounterSinkHole(3.8 / 2.0, 8.0 / 2.0, wall_thickness, mode=Mode.SUBTRACT)

    with Locations(Location((0, 28, top_face.center().Z - 1))): 
        add(pocket2, rotation=(0, 0, 120), mode=Mode.SUBTRACT)

    bottom_face = body_left.faces().sort_by(Axis.Z)[0]
    with Locations(Location((12, -44, bottom_face.center().Z + 1))):
        add(pocket2, rotation=(0, 180, -120), mode=Mode.SUBTRACT)

# %%
show(body_left)
# %%
export_stl(body.part, "multitool_holder.stl")
export_stl(body_left.part, "multitool_holder_left.stl")
export_stl(pocket.part, "multitool_holder_pocket.stl")

# %%
