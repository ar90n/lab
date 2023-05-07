# %%
import cadquery as cq
from jupyter_cadquery.viewer.client import show, show_object
# %%
#box = cq.Workplane("XY").rect(50, 100, centered=False).extrude(30)
box = cq.Workplane("XY").box(50, 100, centered=False)
show(box)

# %%
p0 = cq.Vector(50, 0, 30)
p1 = cq.Vector(0, 100, 20)
p2 = cq.Vector(50, 100, 30)
p10 = p1 - p0
p20 = p2 - p0