# %%
from build123d import *
from ocp_vscode import show, show_object
# %%
with BuildPart() as usb_body:
    Box(51.6, 17.9, 7)

    fillet(usb_body.faces().filter_by(Axis.X).sort_by(Axis.X).last.edges().filter_by(Axis.Z).sort_by(Axis.Y).last, 2.95)
    fillet(usb_body.faces().filter_by(Axis.Y).sort_by(Axis.Y).first.edges().filter_by(Axis.X), 1.45)


    loc = usb_body.faces().filter_by(Axis.X).sort_by(Axis.X).first.center_location
    RigidJoint("usb_axis", joint_location=
               Location(loc.position + Vector(0, 0, -1.2), loc.orientation)
    )

show(usb_body)
# %%
with BuildPart() as angle_body:
    Box(24.2, 12.5, 6.3)
    fillet(angle_body.faces().filter_by(Axis.Y).sort_by(Axis.Y).edges().filter_by(Axis.X), 1.35)

    with BuildSketch(angle_body.faces().filter_by(Axis.Y).sort_by(Axis.Y).first) as connector:
        with Locations((0, -((24.0 - 10.6) / 2 - 0.2))):
            RectangleRounded(6.3, 11.2, 2.5)
    base = extrude(amount=2.1, both=True)
    with BuildSketch(base.faces().filter_by(Axis.Y).sort_by(Axis.Y).first):
        RectangleRounded(2.6, 8.7, 1.3 - 1e-5)
    extrude(amount=6.7)

    RigidJoint("usb_axis", joint_location=-angle_body.faces().filter_by(Axis.X).sort_by(Axis.X).first.center_location)

show(angle_body)
# %%
angle_body.joints["usb_axis"].connect_to(usb_body.joints["usb_axis"])
# %%
inner = usb_body.part + angle_body.part
show(inner)
# %%
with BuildPart() as all_body:
    Box(51.8 + 24.0 + 6 * 2, 17.7 + 2.5, 9.0)
    with Locations((26, -1.8, -0.8)):
        add(inner, mode=Mode.SUBTRACT)

    fillet(all_body.faces().filter_by(Axis.Y).sort_by(Axis.Y).last.edges().filter_by(Axis.Z), 3.0)

    with Locations(all_body.faces().filter_by(Axis.Z).sort_by(Axis.Z).first):
        with Locations(((51.8 + 24.0 + 5.5) / 2, -(17.7 / 2.0 - 1.8))):
            CounterBoreHole(3.1 / 2.0, 5.4 / 2.0, 2.2, 8.5)
        with Locations((-(51.8 + 24.0 + 6.0) / 2, (17.7 / 2.0 - 1.8))):
            CounterBoreHole(3.1 / 2.0, 5.4 / 2.0, 2.2, 8.5)
show(all_body)
# %%
with BuildPart() as bottom_body:
    add(all_body)
    split(bisect_by=(Plane.XY.offset(1)))

    with BuildSketch(Plane.XY.offset(1)):
        with Locations(((51.8 + 24.0 + 5.5) / 2, (17.7 / 2.0 - 1.8))):
            RegularPolygon(radius=5.8 / 2.0, side_count=6, rotation=45)
        with Locations((-(51.8 + 24.0 + 6.0) / 2, -(17.7 / 2.0 - 1.8))):
            RegularPolygon(radius=5.8 / 2.0, side_count=6, rotation=45)
    extrude(amount=1.8, mode=Mode.SUBTRACT)

show(bottom_body)

# %%
with BuildPart() as top_body:
    add(all_body)
    split(bisect_by=(Plane.XY.offset(1)),keep=Keep.BOTTOM)
show(top_body)

# %%
top_body.part.export_stl("usb_mount_top.stl")
bottom_body.part.export_stl("usb_mount_bottom.stl")
# %%
