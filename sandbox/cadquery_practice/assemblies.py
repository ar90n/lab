# %%
import cadquery as cq
from jupyter_cadquery.viewer.client import show, show_object

# %%
H = 400
W = 200
D = 350

PROFILE = cq.importers.importDXF("vslot-2020_1.dxf").wires()

SLOT_D = 5
PANEL_T = 3

HANDLE_D = 20
HANDLE_L = 50
HANDLE_W = 4
# %%
def make_vslot(l):
    return PROFILE.toPending().extrude(l)
# %%
def make_connector():
    rv = (
        cq.Workplane()
        .box(20, 20, 20)
        .faces("<X")
        .workplane()
        .cboreHole(6, 15, 18)
        .faces("<Z")
        .workplane(centerOption="CenterOfMass")
        .cboreHole(6, 15, 18)
    )

    rv.faces(">X").tag("X").end()
    rv.faces(">Z").tag("Z").end()

    return rv
# %%
def make_panel(w, h, t, cutout):

    rv = (
        cq.Workplane("XZ")
        .rect(w, h)
        .extrude(t)
        .faces(">Y")
        .vertices()
        .rect(2*cutout,2*cutout)
        .cutThruAll()
        .faces("<Y")
        .workplane()
        .pushPoints([(-w / 3, HANDLE_L / 2), (-w / 3, -HANDLE_L / 2)])
        .hole(3)
    )

    # tag mating edges
    rv.faces(">Y").edges("%CIRCLE").edges(">Z").tag("hole1")
    rv.faces(">Y").edges("%CIRCLE").edges("<Z").tag("hole2")

    return rv


def make_handle(w, h, r):

    pts = ((0, 0), (w, 0), (w, h), (0, h))

    path = cq.Workplane().polyline(pts)

    rv = (
        cq.Workplane("YZ")
        .rect(r, r)
        .sweep(path, transition="round")
        .tag("solid")
        .faces("<X")
        .workplane()
        .faces("<X", tag="solid")
        .hole(r / 1.5)
    )

    # tag mating faces
    rv.faces("<X").faces(">Y").tag("mate1")
    rv.faces("<X").faces("<Y").tag("mate2")

    return rv
# %%
door = (
    cq.Assembly()
    .add(make_vslot(H), name="left")
    .add(make_vslot(H), name="right")
    .add(make_vslot(W), name="top")
    .add(make_vslot(W), name="bottom")
    .add(make_connector(), name="con_tl", color=cq.Color("black"))
    .add(make_connector(), name="con_tr", color=cq.Color("black"))
    .add(make_connector(), name="con_bl", color=cq.Color("black"))
    .add(make_connector(), name="con_br", color=cq.Color("black"))
    .add(
        make_panel(W + SLOT_D, H + SLOT_D, PANEL_T, SLOT_D),
        name="panel",
        color=cq.Color(0, 0, 1, 0.2),
    )
    .add(
        make_handle(HANDLE_D, HANDLE_L, HANDLE_W),
        name="handle",
        color=cq.Color("yellow"),
    )
)
(
    door
    # left profile
    .constrain("left@faces@<Z", "con_bl?Z", "Plane")
    .constrain("left@faces@<X", "con_bl?X", "Axis")
    .constrain("left@faces@>Z", "con_tl?Z", "Plane")
    .constrain("left@faces@<X", "con_tl?X", "Axis")
    # top
    .constrain("top@faces@<Z", "con_tl?X", "Plane")
    .constrain("top@faces@<Y", "con_tl@faces@>Y", "Axis")
    # bottom
    .constrain("bottom@faces@<Y", "con_bl@faces@>Y", "Axis")
    .constrain("bottom@faces@>Z", "con_bl?X", "Plane")
    # right connectors
    .constrain("top@faces@>Z", "con_tr@faces@>X", "Plane")
    .constrain("bottom@faces@<Z", "con_br@faces@>X", "Plane")
    .constrain("left@faces@>Z", "con_tr?Z", "Axis")
    .constrain("left@faces@<Z", "con_br?Z", "Axis")
    # right profile
    .constrain("right@faces@>Z", "con_tr@faces@>Z", "Plane")
    .constrain("right@faces@<X", "left@faces@<X", "Axis")
    # panel
    .constrain("left@faces@>X[-4]", "panel@faces@<X", "Plane")
    .constrain("left@faces@>Z", "panel@faces@>Z", "Axis")
    # handle
    .constrain("panel?hole1", "handle?mate1", "Plane")
    .constrain("panel?hole2", "handle?mate2", "Point")
)

door.solve()
show(door, name='door')
# %%
cone = cq.Solid.makeCone(1, 0, 2)
assy = cq.Assembly()
assy.add(
    cone,
    loc=cq.Location((0, 0, 0), (1, 0, 0), 180),
    name="cone0",
    color=cq.Color("green")
)
assy.add(cone, name="cone1", color=cq.Color("blue"))

show(assy)
# %%
cone = cq.Solid.makeCone(1, 0, 2)
assy = cq.Assembly()
assy.add(cone, name="cone0", color=cq.Color("green"))
assy.add(cone, name="cone1", color=cq.Color("blue"))
assy.constrain("cone0@faces@<Z", "cone1@faces@<Z", "Axis")
assy.solve()
show(assy)
# %%
bracket = (
    cq.Workplane("YZ")
    .hLine(1)
    .vLine(0.1)
    .hLineTo(0.2)
    .vLineTo(1)
    .hLineTo(0)
    .close()
    .extrude(1)
    # tag some faces for easy reference:
    .faces(">Y[1]")
    .tag("inner_vert")
    .end()
    .faces(">Z[1]")
    .tag("inner_horiz")
    .end()
)

box = cq.Workplane().box(0.5, 0.5, 0.5)

assy = cq.Assembly()
assy.add(bracket, name="bracket", color=cq.Color("gray"))
assy.add(box, name="box", color=cq.Color("green"))

# lock bracket orientation:
assy.constrain("bracket@faces@>Z", "box@faces@>Z", "Axis", param=0)
assy.constrain("bracket@faces@>X", "box@faces@>X", "Axis", param=0)

# constrain the bottom of the box to be on the plane defined by inner_horiz:
assy.constrain("box@faces@<Z", "bracket?inner_horiz", "PointInPlane")
# constrain the side of the box to be 0.2 units from the plane defined by inner_vert
assy.constrain("box@faces@<Y", "bracket?inner_vert", "PointInPlane", param=0.2)
# constrain the end of the box to be 0.1 units inside the end of the bracket
assy.constrain("box@faces@>X", "bracket@faces@>X", "PointInPlane", param=-0.1)

assy.solve()
show(assy)
# %%
bracket = (
    cq.Workplane("YZ")
    .hLine(1)
    .vLine(0.1)
    .hLineTo(0.2)
    .vLineTo(1)
    .hLineTo(0)
    .close()
    .extrude(1)
    # tag some faces for easy reference:
    .faces(">Y[1]")
    .tag("inner_vert")
    .end()
    .faces(">Z[1]")
    .tag("inner_horiz")
    .end()
)
show(bracket)
# %%
