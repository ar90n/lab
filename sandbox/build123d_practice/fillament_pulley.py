# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
pipe_diameter = 8.0
pipe_clearance = 0.6
ptfe_tube_diameter = 4.2
ptfe_tube_clearance = 0.3
margin = 2.0
pulley_inner_diameter = pipe_diameter + 2.0 * (ptfe_tube_diameter + margin)
pulley_flat_half_thickness = 3.0 / 2.0
pulley_angled_thickness = 6.0
pulley_outer_diameter = pulley_inner_diameter + 2.0 * 3.0
pulley_edge_width = 1.5

# %%
with BuildPart() as pulley:
    with BuildSketch():
        Circle(pulley_outer_diameter / 2.0)
    extrude(amount=pulley_edge_width)

    edge_topf = pulley.faces().sort_by(Axis.Z).last
    add(edge_topf)
    with BuildSketch(Plane(edge_topf).offset(pulley_angled_thickness)):
        Circle(pulley_inner_diameter / 2.0)
    loft()
    with BuildSketch(Plane(edge_topf).offset(pulley_angled_thickness)):
        Circle(pulley_inner_diameter / 2.0)
    extrude(amount=pulley_flat_half_thickness)

    outer_f = pulley.faces().sort_by(Axis.Z).first
    with Locations(outer_f.center_location):
        Hole((pipe_diameter + pipe_clearance) / 2.0)
        #with PolarLocations((pipe_diameter + ptfe_tube_diameter) / 2.0, 3, 0):
        #    Hole((ptfe_tube_diameter + ptfe_tube_clearance) / 2.0 )
    
    center_f = pulley.faces().sort_by(Axis.Z).last
    mirror(about=Plane(center_f))
show(pulley)

# %%
pulley.part.export_stl("pulley.stl")
# %%
