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
    #(0, d + 1.4),
    #(2.3, d + 1.4),
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
    (2.3 + 20.2 - 4.0 - 1.0 - 16.5 + d, d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5 + dd),
    (2.3 + 20.2 + d + 0        , d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5 + dd),
    (2.3 + 20.2 + d + 6        , d + 7.2 + 1.3 + 2.7 + 1.3 + 7.5 + dd - 0.1),
    (2.3 + 20.2 + d + 6, 0.0),
]
pts2 = [
    (5, 0),
    (21.5, 0),
    (21.5, 2.3),
    (14.5, 2.3),
    (14.5, 2.3 + 13.7),
    (21.5, 2.3 + 13.7),
    (21.5, 2.3 + 13.7 + 10),
    (5,    2.3 + 13.7 + 10),
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

    front_face = part.faces().sort_by(Axis.Y)[-1]
    eee = front_face.edges().filter_by(Axis.X).sort_by(Axis.Z)[-1]
    with BuildSketch(Location(eee @ 0)) as sk2:
        with BuildLine() as ln2:
            l1 = Polyline(*pts2)
            Line(l1 @ 1, l1 @ 0)
        make_face()
    extrude(amount=-ll2)

    for e in [part.edges().filter_by(Axis.Z).sort_by(Axis.Y)[-1],
        part.edges().filter_by(Axis.Z).sort_by(Axis.Y)[-2],
        part.edges().filter_by(Axis.Z).sort_by(Axis.Y)[-3],
        part.edges().filter_by(Axis.Z).sort_by(Axis.Y)[-4],
        part.edges().filter_by(Axis.Z).sort_by(Axis.Y)[-6]
    ]:
        chamfer(e, length=1.0)

    for e in es:
        chamfer(e.group_by(Axis.X)[-1], length=1.0)

    import math
    with BuildSketch(front_face) as sk3:
        with Locations((-1.05, -4.8, 0.0)) as loc3:
            Circle(2.0)
        with Locations((-1.05, 9.0, 0.0)) as loc3:
            Circle(2.0)
    extrude(amount=1.2)

    for e in part.edges().filter_by(GeomType.CIRCLE):
        chamfer(e, length=0.05)


    with BuildSketch(Plane.XY) as sk4:
        Rectangle(2.3 + 20.2, d, align=(Align.MIN, Align.MIN))
    extrude(amount=(l - 15), mode=Mode.SUBTRACT)

    with BuildSketch(Plane.XY) as sk4:
        Rectangle(2.3 + 20.2, d + 20, align=(Align.MIN, Align.MIN))
    extrude(amount=(l - 20), mode=Mode.SUBTRACT)


show(part.part)
# %%
part.part.export_stl("part.stl")
# %%
