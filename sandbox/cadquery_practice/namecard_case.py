# %%
import cadquery as cq
import numpy as np
from jupyter_cadquery.viewer.client import show, show_object

# %%
NAMECARD_WIDTH = 92
NAMECARD_HEIGHT = 56
NAMECARD_DEPTH = 5
HEIGHT_MARGIN = 3
WIDTH_MARGIN = 2
DEPTH_MARGIN = 2
EPS= 0.15
# %%
back = cq.Workplane("XZ").box(
    NAMECARD_HEIGHT + 2 * HEIGHT_MARGIN,
    NAMECARD_DEPTH + 2 * DEPTH_MARGIN,
    WIDTH_MARGIN
)

bottom = (
    back.faces("<Y")
    .center(0, -(NAMECARD_DEPTH + 2 * DEPTH_MARGIN) / 2.0)
    .workplane()
    .hLine(NAMECARD_HEIGHT / 2.00 + HEIGHT_MARGIN)
    .vLine(DEPTH_MARGIN)
    .hLine(-HEIGHT_MARGIN * 2 / 3)
    .vLine(NAMECARD_DEPTH / 2 - HEIGHT_MARGIN * 1 / 6)
    #.hLine(HEIGHT_MARGIN * 1 / 3)
    .line(HEIGHT_MARGIN * 1 / 3 - EPS, HEIGHT_MARGIN * 1 / 3)
    .vLine(NAMECARD_DEPTH / 2 - HEIGHT_MARGIN * 1 / 6)
    .hLine(-HEIGHT_MARGIN * 2 / 3 + EPS)
    .vLine(-NAMECARD_DEPTH)
    .hLineTo(0)
    .mirrorY()
    .extrude(NAMECARD_WIDTH)
)
base = back + bottom
show(base)

# %%
top = (back.faces(">Y")
    .center(0, -(NAMECARD_DEPTH + 2 * DEPTH_MARGIN) / 2.0)
    .workplane()
    .hLine(NAMECARD_HEIGHT / 2.00 + HEIGHT_MARGIN + 2 * EPS)
    .vLine(DEPTH_MARGIN)
    .vLine(NAMECARD_DEPTH)
    .hLine(-HEIGHT_MARGIN * 2 / 3 + EPS)
    .vLine(-NAMECARD_DEPTH / 2 + HEIGHT_MARGIN * 1 / 6)
    .line(HEIGHT_MARGIN * 1 / 3 - EPS, - HEIGHT_MARGIN/ 3)
    .vLine(-NAMECARD_DEPTH / 2 +  HEIGHT_MARGIN * 1 / 6)
    .hLineTo(0)
    .mirrorY()
    .extrude(NAMECARD_WIDTH * 0.9)
)
cover = top + back
show(cover)
# %%
assy = cq.Assembly()
assy.add(base, name="base")
assy.add(cover, name="cover")
assy.constrain("base@faces@<Z", "cover@faces@<Z", "Axis")
assy.constrain("base@faces@<Y", "cover@faces@>Y", "Axis")
assy.constrain("base@faces@>Y[1]", "cover@faces@>Y", "PointInPlane")
assy.constrain("base@faces@<Z[1]", "cover@faces@>Z[1]", "PointInPlane")
assy.constrain("base@faces@<X[1]", "cover@faces@>X[1]", "PointInPlane")
assy.solve()
show(assy)
# %%
cq.exporters.export(base, "base.stl")
cq.exporters.export(cover, "cover.stl")
# %%
