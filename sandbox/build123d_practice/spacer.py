# %%
from build123d import *
from ocp_vscode import show
# %%
rot = Rectangle(width=16, height=16)
for i, p in enumerate(GridLocations(8.0, 8.0, 2, 2, align=Align.CENTER)):
        rot -= p * Circle(1.6)

spacer = extrude(rot, 3.5)
spacer -= Pos(0, -8, 3.5) *Box(16, 1.8, 2.5, align=[Align.CENTER, Align.MIN, Align.MAX])

show(spacer)
# %%
spacer.export_stl("spacer.stl")
# %%
