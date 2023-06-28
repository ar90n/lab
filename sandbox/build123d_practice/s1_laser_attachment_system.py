# %%
from build123d import *
from ocp_vscode import show, show_object

# %%
d = 4
dd = 8
ll2 = 2.8
l = 45 + ll2

pts = [
    (0, 0),
    (0, d),
    (2.3, d),
    (2.3 + 20.2, d),
    (2.3 + 20.2, d + 7.2),
    (2.3 + 20.2 - 3.0, d + 7.2),
    (2.3 + 20.2 - 3.0, d + 7.2 + 1.3),
    (2.3 + 20.2, d + 7.2 + 1.3),
    (2.3 + 20.2, d + 7.2 + 1.3 + 2.7),
    (2.3 + 20.2 - 3.0, d + 7.2 + 1.3 + 2.7),
    (2.3 + 20.2 - 3.0, d + 7.2 + 1.3 + 2.7 + 1.3),
    (2.3 + 20.2, d + 7.2 + 1.3 + 2.7 + 1.3),
    (2.3 + 20.2, d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5),
    (2.3 + 20.2 - 4.0, d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5),
    (2.3 + 20.2 - 4.0 - 1.0, d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5),
    (2.3 + 20.2 - 4.0 - 1.0 - 16.5 + d, d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5),
    (2.3 + 20.2 - 4.0 - 1.0 - 16.5 + d, d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5 + d),
    (2.3 + 20.2 - 8 + d, d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5 + d),
    (2.3 + 20.2 - 8 + d, d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5 + d + 2.54),
    (2.3 + 20.2 + d + 0               , d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5 + d + 2.54),
    (2.3 + 20.2 + d + 0, 0.0),
]

with BuildPart() as part:
    with BuildSketch(Plane.XY) as sk:
        with BuildLine() as ln:
            l1 = Polyline(*pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
    extrude(amount=l)
    es = [
        part.edges().filter_by(Axis.Z).group_by(Axis.Y)[-7],
        part.edges().filter_by(Axis.Z).group_by(Axis.Y)[-4],
    ]
    for e in es:
        chamfer(e.group_by(Axis.X)[-1], length=1.0)

    import math
    front_face = part.faces().sort_by(Axis.Y)[-1]
    with BuildSketch(front_face) as sk3:
        with Locations((0, -6.8, 0.0), (0.0, 6.8, 0.0)) as loc3:
            RegularPolygon(radius=3.25, side_count=6, rotation = 180 / 6)
    extrude(amount=-2.4, mode=Mode.SUBTRACT)

    with BuildSketch(front_face) as sk4:
        with Locations((0, -6.8, 0.0), (0.0, 6.8, 0.0)) as loc3:
            Circle(1.4)
    extrude(amount=-4.5, mode=Mode.SUBTRACT)

show(part.part)
# %%
part.part.export_stl("part_laser.stl")
# %%
