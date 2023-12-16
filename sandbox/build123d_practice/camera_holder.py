# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
camera_width = 58.2
camera_height = 58.2
camera_center_diameter = 44.0
camera_min_depth = 22.3
camera_max_depth = 30.7
camera_chamfer_radius = 14.8 / 2.0

# %%
camera_slope_depth = (camera_max_depth - camera_min_depth) / 2.0
# %%
with BuildPart() as camera_body:
    with BuildSketch(Plane.XY):
        Circle(camera_center_diameter / 2.0)
    with BuildSketch(Plane.XY.offset(camera_slope_depth)):
        Rectangle(camera_width, camera_height)
    tmp = loft()
    tmp = extrude(tmp.faces().sort_by(Axis.Z).last, amount = camera_min_depth / 2.0)
    mirror(about=Plane(tmp.faces().sort_by(Axis.Z).last))
    fillet(camera_body.edges().filter_by(Axis.Z), radius=camera_chamfer_radius)

# %%
camera_cover_wall_thickness = 2.0
camera_cover_slot_margin = 2.0
camera_cover_center_diameter = camera_center_diameter + 2.0 * camera_cover_wall_thickness
camera_cover_width = camera_width + 2.0 * camera_cover_wall_thickness
camera_cover_height = camera_cover_width
camera_cover_slope_depth = camera_slope_depth

# %%
with BuildPart() as camera_cover:
    with BuildSketch(Plane.XY.offset(-camera_cover_wall_thickness)):
        Circle(camera_cover_center_diameter / 2.0)
    with BuildSketch(Plane.XY.offset(camera_cover_slope_depth)):
        Rectangle(camera_cover_width, camera_cover_height)
    tmp = loft()
    tmp = extrude(tmp.faces().sort_by(Axis.Z).last, amount = camera_min_depth / 2.0)
    mirror(about=Plane(tmp.faces().sort_by(Axis.Z).last))
    fillet(camera_cover.edges().filter_by(Axis.Z), radius=7.4 / 2.0)

    leftf = camera_cover.faces().sort_by(Axis.X).first
    _, x_spacing, y_spacing = leftf.bounding_box().size
    with Locations(leftf):
        for i, loc in enumerate(GridLocations(x_spacing / 2.0, y_spacing / 2.0, 2, 1)):
            RevoluteJoint(
                label=f"left_holder_{i}",
                axis=Axis(loc.position, (0.0, 1.0, 0.0)),
            )

    rightf = camera_cover.faces().sort_by(Axis.X).last
    _, x_spacing, y_spacing = rightf.bounding_box().size
    with Locations(rightf):
        for i, loc in enumerate(GridLocations(x_spacing / 2.0, y_spacing / 2.0, 2, 1)):
            RevoluteJoint(
                label=f"right_holder_{i}",
                axis=Axis(loc.position, (0.0, -1.0, 0.0)),
            )

    with BuildSketch(camera_cover.faces().sort_by(Axis.Y).last):
        slot_width = camera_width - 2.0 * camera_cover_slot_margin
        slot_height = camera_min_depth - 2.0 * camera_cover_slot_margin
        Rectangle(slot_width, slot_height)
    extrude(amount=-10, mode=Mode.SUBTRACT)

    with BuildSketch(camera_cover.faces().sort_by(Axis.Z).last):
        Circle(camera_center_diameter / 2.0 - camera_cover_wall_thickness)
    extrude(amount=-1000, mode=Mode.SUBTRACT)

# %%
def Holder(bottom: float, top: float, width: float, is_left: bool = True):
    height = (bottom - top) / 2.0
    pts = [
        (-bottom / 2.0, 0.0),
        (-top / 2.0, height),
        (top/ 2.0, height),
        (bottom/ 2.0, 0.0),
    ]
    with BuildPart() as holder:
        with BuildSketch():
            with BuildLine():
                ls = Polyline(pts)
                Line(ls @ 0, ls @ 1)
            make_face()

        extrude(amount=width)
        fillet(holder.faces().sort_by(Axis.Y).last.edges().filter_by(Axis.Z), radius=top)

        with Locations(holder.faces().sort_by(Axis.Z).first):
            CounterBoreHole(3.0 / 2.0, 5.7 / 2.0, 7.0)

        RigidJoint(
            label="hole",
            joint_location=Location(
                holder.faces().sort_by(Axis.Z).last.center_location.position,
                holder.faces().sort_by(Axis.Z).last.center_location.orientation,
            )
        )
        print(holder.faces().sort_by(Axis.Z).last.center_location.orientation,)

        RigidJoint(
            label="base",
            joint_location=holder.faces().sort_by(Axis.Y).first.center_location
        )
    return holder

# %%
_, _, y_spacing = camera_cover.faces().sort_by(Axis.X).first.bounding_box().size
h0 = Holder(20.0, 5.0, y_spacing)
camera_cover.part.joints["left_holder_0"].connect_to(h0.joints["base"], angle=0)
h1 = Holder(20.0, 5.0, y_spacing)
camera_cover.part.joints["left_holder_1"].connect_to(h1.joints["base"], angle=0)
h2 = Holder(20.0, 5.0, y_spacing, is_left=False)
camera_cover.part.joints["right_holder_0"].connect_to(h2.joints["base"], angle=180)
h3 = Holder(20.0, 5.0, y_spacing, is_left=False)
camera_cover.part.joints["right_holder_1"].connect_to(h3.joints["base"], angle=180)
holders = [h0, h1, h2, h3]
# %%
with BuildPart() as cover:
    add(camera_cover)
    add(camera_body, mode=Mode.SUBTRACT)
    for i, h in enumerate(holders):
        add(h)
        RigidJoint(
            label=f"hole_{i}",
            joint_location=h.location * h.joints["hole"].relative_location 
        )
# %%
cover.part.export_stl("camera_cover.stl")
# %%
show(cover, render_joints=True, show_parent=True)
# %%
battery_width = 64.0
battery_depth = 15.6
battery_height = 77.0
battery_corner_radius = 9.8

# %%
clearance = 0.1
battery_cover_thickness = 2.0
battery_cover_width = battery_width + 2.0 * battery_cover_thickness + 2.0 * clearance
battery_cover_depth = battery_depth + 2.0 * battery_cover_thickness + 2.0 * clearance
battery_cover_height = battery_height + 2.0 * battery_cover_thickness + 2.0 * clearance
battery_cover_corner_radius = battery_corner_radius + battery_cover_thickness + clearance

# %%
with BuildPart() as battery_cover_body:
    Box(battery_cover_width, battery_cover_depth, battery_cover_height)
    topf = battery_cover_body.faces().sort_by(Axis.Z).last
    offset(amount=-battery_cover_thickness, openings=[topf])

    backf = battery_cover_body.faces().sort_by(Axis.Y).last
    fillet(backf.edges().filter_by(Axis.Z), radius=battery_cover_thickness)

    leftf = battery_cover_body.faces().sort_by(Axis.X).first
    _, y_spacing, x_spacing = leftf.bounding_box().size
    with Locations(leftf):
        for i, loc in enumerate(GridLocations(x_spacing / 2.0, y_spacing / 2.0, 2, 1)):
            RevoluteJoint(
                label=f"left_holder_{i}",
                axis=Axis(loc.position, (0.0, 90.0, 0.0)),
            )

    rightf = battery_cover_body.faces().sort_by(Axis.X).last
    _, y_spacing, x_spacing = rightf.bounding_box().size
    with Locations(rightf):
        for i, loc in enumerate(GridLocations(x_spacing / 2.0, y_spacing / 2.0, 2, 1)):
            RevoluteJoint(
                label=f"right_holder_{i}",
                axis=Axis(loc.position, (0.0, -90.0, 0.0)),
            )

_, x_spacing, _ = battery_cover_body.faces().sort_by(Axis.X).first.bounding_box().size
battery_cover_holders = [
Holder(20.0, 5.0, x_spacing)
,Holder(20.0, 5.0, x_spacing)
,Holder(20.0, 5.0, x_spacing, is_left=False)
,Holder(20.0, 5.0, x_spacing, is_left=False)
]
battery_cover_body.part.joints["left_holder_0"].connect_to( battery_cover_holders[0].joints["base"], angle=90)
battery_cover_body.part.joints["left_holder_1"].connect_to( battery_cover_holders[1].joints["base"], angle=90)
battery_cover_body.part.joints["right_holder_0"].connect_to(battery_cover_holders[2].joints["base"], angle=90)
battery_cover_body.part.joints["right_holder_1"].connect_to(battery_cover_holders[3].joints["base"], angle=90)

with BuildPart() as battery_cover:
    add(battery_cover_body)
    for i, h in enumerate(battery_cover_holders):
        print(i, h.location)
        add(h)
        RigidJoint(
            label=f"hole_{i}",
            joint_location=h.location * h.joints["hole"].relative_location 
        )

show(battery_cover, render_joints=True, show_parent=True)
# %%
battery_cover_body.location
# %%
battery_cover.part.export_stl("battery_cover.stl")
# %%
base_board_width = 180.0
base_board_height = 110.0
base_board_depth = 7.0

battery_cover_hole_locs = [battery_cover.location * battery_cover.joints[f"hole_{i}"].relative_location for i in range(4)]
cover_hole_locs = [cover.location * cover.joints[f"hole_{i}"].relative_location for i in range(4)]
# %%
with BuildPart() as base_board:
    Box(base_board_height, base_board_width, base_board_depth)
    fillet(base_board.edges().filter_by(Axis.Z), radius=5.0)

    topf = base_board.faces().sort_by(Axis.Z).last
    bottomf = base_board.faces().sort_by(Axis.Z).first
    _, _, tz = topf.center_location.position
    _, _, bz = bottomf.center_location.position
    for i, org_loc in enumerate(battery_cover_hole_locs):
        y, _, x = org_loc.position.to_tuple()
        tloc = Location(Vector(x, y + 42.5, tz), (180, 0, 0))
        RigidJoint(
            label=f"battery_cover_hole_{i}",
            joint_location=tloc
        )
        bloc = Location(Vector(x, y + 42.5, bz), (0, 0, 0))
        with Locations(bloc):
            Hole(3.0 / 2.0)
        with BuildSketch(bloc):
            RegularPolygon(radius=3.1, side_count=6)
        extrude(amount=4.4, mode=Mode.SUBTRACT)

    avg_x = 0.0
    avg_y = 0.0
    for i, org_loc in enumerate(cover_hole_locs):
        y, x, _ = org_loc.position.to_tuple()
        avg_x += x
        avg_y += y - 42.5
        tloc = Location(Vector(x, y - 42.5, tz), (180, 0, 0))
        RigidJoint(
            label=f"cover_hole_{i}",
            joint_location=tloc
        )

        bloc = Location(Vector(x, y - 42.5, bz), (180, 0, 0))
        with Locations(bloc):
            Hole(3.0 / 2.0)
        with BuildSketch(bloc):
            RegularPolygon(radius=3.1, side_count=6)
        extrude(amount=-4.5, mode=Mode.SUBTRACT)

    cloc = Location(Vector(avg_x / 4.0, avg_y / 4.0, tz), (0, 0, 0))
    with BuildSketch(cloc):
        Rectangle(59.5, 59.5)
        Rectangle(18, 64)
    hh = extrude(amount=-20, mode=Mode.SUBTRACT)
    chamfer(hh.edges().group_by(Axis.Z)[-1], length=2.0, angle=60.0)



# %%
for i in range(4):
    base_board.joints[f"battery_cover_hole_{i}"].connect_to(battery_cover.joints[f"hole_{i}"])
for i in range(4):
    base_board.joints[f"cover_hole_{i}"].connect_to(cover.joints[f"hole_{i}"])
bb_symbols = [base_board.joints[f"battery_cover_hole_{i}"].symbol for i in range(4)]
bc_symbols = [battery_cover.joints[f"hole_{i}"].symbol for i in range(4)]
c_symbols = [cover.joints[f"hole_{i}"].symbol for i in range(4)]
show(battery_cover, cover,  base_board, *bb_symbols, *bc_symbols, *c_symbols)

# %%
base_board.part.export_stl("base_board.stl")
# %%
