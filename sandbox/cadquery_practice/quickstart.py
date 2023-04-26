# %%
import cadquery as cq
from jupyter_cadquery.viewer.client import show, show_object
# %%
height = 60.0
width = 80.0
thickness = 10.0
diameter = 22.0
padding = 12.0

# %%
result = (cq.Workplane("XY")
    .box(height, width, thickness)
    .faces(">Z").workplane().hole(diameter)
    .faces(">Z").workplane()
    .rect(height - padding, width - padding, forConstruction=True)
    .vertices().cboreHole(2.4, 4.4, 2.1)
    .edges("|Z").fillet(2.0)
)
show(result)