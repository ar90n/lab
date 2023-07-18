# %%
import pickle
from pathlib import Path

# %%
import numpy as np
from build123d import *
from ocp_vscode import show

# %%
runa_points_path = Path.cwd() / "runa_points.pickle"
data = pickle.loads(runa_points_path.read_bytes())
inner = np.array(data["inner"])
inner = inner - np.mean(inner, axis=0)
outer = np.array(data["outer"])
outer = outer - np.mean(outer, axis=0)
# %%
mm_over_pix = 57 / (np.max(inner[:,0]) - np.min(inner[:,0]))
mm_data_inner = mm_over_pix * inner
mm_data_outer = mm_over_pix * outer

# %%
inner_pts = [(float(x), float(y)) for x, y in mm_data_inner]
outer_pts = [(float(x), float(y)) for x, y in mm_data_outer]
# %%
with BuildPart() as part:
    with BuildSketch(Plane.XY) as outer_sk:
        with BuildLine() as outer_ln:
            l1 = Polyline(*outer_pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
        
        with Locations(outer_ln.edges().last.vertices()[0].to_tuple()):
            Circle(3)
    extrude(amount=5)
    with BuildSketch(Plane.XY) as inner_sk:
        with BuildLine() as inner_ln:
            l1 = Polyline(*inner_pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
        with Locations(outer_ln.edges().last.vertices()[0].to_tuple()):
            Circle(2.5)
    extrude(amount=5, mode=Mode.SUBTRACT)

# %%
part.part
# %%
part.part.export_stl("runa_cast.stl")
# %%
