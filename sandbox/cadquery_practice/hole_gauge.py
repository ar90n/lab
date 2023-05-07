# %%
import cadquery as cq
from jupyter_cadquery.viewer.client import show, show_object


# %%
rs = (float(1.0 + i * 0.5) for i in range(1, 10))
gauge = (
    cq.Workplane("XY")
    .box(60.0, 60.0, 5)
    .edges("|Z")
    .fillet(5)
    .faces(">Z")
    .chamfer(0.5)
    .faces(">Z")
    .workplane()
    .rarray(15.0,15.0, 3, 3)
    .eachpoint(lambda loc: (cq.Workplane().circle(next(rs)).val().located(loc)))
    .cutBlind("last")
)
show(gauge)