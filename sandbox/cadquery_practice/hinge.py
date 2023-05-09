# %%
import cadquery as cq
from jupyter_cadquery.viewer.client import show, show_object

# %%
# %%
holder = (
    cq.Workplane("XZ")
    .cylinder(6, 3)
    .faces(">Y")
    .workplane()
    .cskHole(1e-4, 5, 90, depth=2.5)
    .faces("<Y")
    .workplane()
    .cskHole(1e-4, 5, 90, depth=2.5)
    .copyWorkplane(cq.Workplane("YZ", origin=(2.5, 0, 0)))
    .box(6, 2, 2.5, centered=(True, True, False))
)
# %%
holder_base = (
    cq.Workplane("XY")
    .rarray(1e-12, 13.0, 1, 3)
    .eachpoint(lambda loc: holder.val().located(loc))
    .copyWorkplane(cq.Workplane("XY", origin=(3.5, 0, 0)))
    .box(15, 6.5 * 5, 2, centered=(False, True, True))
)
# %%
hinge = (
    cq.Workplane("XZ")
    .cylinder(6, 3)
    .faces(">Y")
    .workplane()
    .circle(2.0)
    .workplane(offset=2.0)
    .circle(0.1)
    .loft(combine=True)
    .faces("<Y")
    .workplane()
    .circle(2.0)
    .workplane(offset=2.0)
    .circle(0.1)
    .loft(combine=True)
    .copyWorkplane(cq.Workplane("YZ", origin=(2.5, 0, 0)))
    .box(6, 2, 2.5, centered=(True, True, False))
)
# %%
hinge_base = (
    cq.Workplane("XY")
    .rarray(1e-12, 13.0, 1, 2)
    .eachpoint(lambda loc: hinge.val().located(loc))
    .copyWorkplane(cq.Workplane("XY", origin=(3.5, 0, 0)))
    .box(15, 6.5 * 5, 2, centered=(False, True, True))
)
# %%
result = holder_base + hinge_base.mirror("YZ")
# %%
cq.exporters.export(result, "hinge.stl")