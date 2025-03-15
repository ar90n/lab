# %%
from build123d import *
from ocp_vscode import show
import math

# %%
# Wall Mount Model Parameters
# This model creates a cylindrical base with slots to hold plastic walls
# The walls can be aligned in 8 different directions (at 45-degree intervals)

# Parameters
wall_thickness = 3.0  # Thickness of the plastic walls to be mounted [mm]
base_radius = 20.0  # Radius of the cylindrical base [mm]
base_height = 15.0  # Height of the base [mm]
slot_depth = 15.0  # Depth of the slots [mm]
slot_height = 15.0  # Height of the slots above the base [mm]
slot_insert_depth = 3.0  # How deep the slot goes into the base [mm]
slot_clearance = 1.0
num_directions = 8  # Number of directions (slots)

# %%
# Calculate the actual slot width with clearance
slot_width = wall_thickness

# Build the wall mount
with BuildPart() as wall_mount:
    # Create the cylindrical base
    with BuildSketch():
        Circle(base_radius)
    extrude(amount=base_height)
    
    # Create 8 slots around the base at 45-degree intervals
    for i in range(num_directions):
        # Calculate the angle for this slot
        angle = i * (360 / num_directions)
        
        rotated_plane = Plane.XY.rotated((0, 0, angle)).offset(base_height)
        with BuildSketch(rotated_plane):
            # Create a slot that extends both inward and outward from the edge
            # The slot width is set to accommodate the wall thickness plus clearance
            Rectangle(slot_width, base_radius, align=(Align.CENTER, Align.MIN))
        # Extrude the slot upward (including the insert depth into the base)
        # This creates a slot that extends both above and into the base
    extrude(amount=-(base_height - slot_clearance), mode=Mode.SUBTRACT)
    fillet(wall_mount.edges().filter_by(Axis.Z), radius=1.0)

# %%
show(wall_mount)
# %%
export_stl(wall_mount.part, 'wall_mount.stl')
# %%
