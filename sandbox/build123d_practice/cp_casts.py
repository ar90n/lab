# %%
import pickle
from pathlib import Path

# %%
import numpy as np
from build123d import *
from ocp_vscode import show

# %%
cp_points = pickle.loads(Path("cp_casts.pickle").read_bytes())
# %%
for i, data in enumerate(cp_points):
    inner = np.array(data["inner"])
    inner = inner - np.mean(inner, axis=0)
    outer = np.array(data["outer"])
    outer = outer - np.mean(outer, axis=0)
    inner = inner[::2,:]

    outer = outer[::2,:]

    inner_pts = [(float(x), float(y)) for x, y in inner]
    outer_pts = [(float(x), float(y)) for x, y in outer]

    with BuildPart() as part:
        with BuildSketch(Plane.XY) as plate:
            Rectangle(65, 50)
        extrude(amount=1)

        with BuildSketch(Plane.XY) as outer_sk:
            with BuildLine() as outer_ln:
                l1 = Polyline(*outer_pts)
                #l2 = Line(l1 @ 1, l1 @ 0)
            make_face()

            #with Locations(outer_ln.edges().last.vertices()[0].to_tuple()):
            #    Circle(3)
        extrude(amount=5)
        with BuildSketch(Plane.XY) as inner_sk:
            with BuildLine() as inner_ln:
                l1 = Polyline(*inner_pts)
    #            l2 = Line(l1 @ 1, l1 @ 0)
            make_face()
            #with Locations(outer_ln.edges().last.vertices()[0].to_tuple()):
            #    Circle(2.5)
        extrude(amount=5, mode=Mode.SUBTRACT)
    part.part.export_stl(f"cp_cast_{i}.stl")
# %%
