# %%
import cadquery as cq
from jupyter_cadquery.viewer.client import show, show_object
# %%
#box = cq.Workplane("XY").rect(50, 100, centered=False).extrude(30)
box = cq.Workplane("XY").box(50, 100, 30, centered=False)
show(box)

# %%
p0 = cq.Vector(50, 0, 30)
p1 = cq.Vector(0, 100, 20)
p2 = cq.Vector(50, 100, 30)
p10 = p1 - p0
p20 = p2 - p0
norm = p10.cross(p20).normalized()

# %%
box = box.split(cq.Face.makePlane(None, None, p0, norm)).solids("<<Z")
# %%
body = cq.Workplane("XY").polarArray(0, 0, 360, 8).each(lambda x: box.translate((-25, 0, 0)).val().located(x))
body = body.union(body.mirror("XY"))