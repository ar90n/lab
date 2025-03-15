# %%
from build123d import *
from ocp_vscode import show
import numpy as np

# %%
ac_dc_box_width = 153.0
ac_dc_box_height = 75.0
ac_dc_box_depth = 26.0
ac_dc_box_fillet = 1.0
ac_dc_box_pillar_radius = 5.0
ac_plug_socket_width = 34.0
ac_plug_socket_height = ac_dc_box_depth
dc_cable_connection_radius = 7.0

ac_dc_box_mount_thickness = 2.54
ac_dc_box_mount_clearance = 0.5

half_wire_internval = 47.5 / 2.0
holder_wire_radius = 3.2 / 2.0
holder_height = 4.2
half_holder_width = half_wire_internval + 2 * holder_wire_radius + 1.0
holder_depth = 34.0

outer_width = ac_dc_box_width + 2 * ac_dc_box_mount_thickness
outer_height = ac_dc_box_depth + 2 * ac_dc_box_mount_thickness
outer_depth = ac_dc_box_height + ac_dc_box_mount_thickness
# %%
with BuildPart() as attachment_part:
    with BuildSketch():
        Rectangle(outer_width / 2.0, holder_height, align=(Align.MIN, Align.CENTER))
        hole_pos = (half_wire_internval + holder_wire_radius, holder_wire_radius * 0.3)
        with Locations(hole_pos):
            radius = holder_wire_radius + ac_dc_box_mount_clearance * 0.5
            Circle(radius, mode=Mode.SUBTRACT)
        for (slit_pos, a) in [((half_wire_internval - 1.3, holder_height / 2.0), Align.MAX), ((half_wire_internval + 2 * holder_wire_radius + 1.3, holder_height / 2.0), Align.MIN) ]:
            with Locations(slit_pos):
                Rectangle(holder_height / 4.0, 2 * holder_wire_radius, align=(a, Align.MAX), mode=Mode.SUBTRACT)
    extrude(amount=outer_height / 2.0, both=True)
    mirror(about=Plane.YZ)
# %%
with BuildPart() as ac_dc_box_mount:
    Box(outer_width, outer_height, outer_depth)

    top_plane = ac_dc_box_mount.faces().sort_by(Axis.Z)[-1]
    with BuildSketch(top_plane):
        Rectangle(ac_dc_box_width, ac_dc_box_depth)
    extrude(amount=-ac_dc_box_height, mode=Mode.SUBTRACT)


    back_plane = ac_dc_box_mount.faces().sort_by(Axis.X)[-2]
    with BuildSketch(back_plane):
        radius = dc_cable_connection_radius + ac_dc_box_mount_clearance
        Circle(radius)
        Rectangle(2 * radius, ac_dc_box_depth / 2.0, align=(Align.CENTER, Align.MAX))
    extrude(amount=-ac_dc_box_mount_thickness, mode=Mode.SUBTRACT)

    front_plane = ac_dc_box_mount.faces().sort_by(Axis.X)[1]
    with BuildSketch(front_plane):
        width = ac_plug_socket_height + ac_dc_box_mount_clearance
        height = ac_plug_socket_width + ac_dc_box_mount_clearance
        Rectangle(width, height)
    extrude(amount=-ac_dc_box_mount_thickness, mode=Mode.SUBTRACT)

    side_plane = ac_dc_box_mount.faces().sort_by(Axis.Y)[0]
    with BuildSketch(side_plane):
        bottom_edge = side_plane.edges().filter_by(Axis.X).sort_by(Axis.Z)[0]
        print(dir(bottom_edge))
        with Locations(bottom_edge.center()):
            Circle(radius)
    extrude(amount=-ac_dc_box_mount_thickness, mode=Mode.ADD)

    split(bisect_by=Plane.XY.offset(10.0), keep=Keep.BOTTOM)

    side_planes = [
        ac_dc_box_mount.faces().sort_by(Axis.Y)[0],
        ac_dc_box_mount.faces().sort_by(Axis.Y)[-1]
    ]
    for side_plane in side_planes:
        with BuildSketch(side_plane):
            with Locations((-22, 0)):
                Rectangle(ac_dc_box_height, ac_dc_box_width * 0.7, align=(Align.MIN, Align.CENTER))
        extrude(amount=-ac_dc_box_mount_thickness, mode=Mode.SUBTRACT)

    top_side_edges = [
        ac_dc_box_mount.faces().sort_by(Axis.X).sort_by(Axis.Z)[-1].edges().filter_by(Axis.Y).sort_by(Axis.X)[0],
        ac_dc_box_mount.faces().sort_by(Axis.X).sort_by(Axis.Z)[-2].edges().filter_by(Axis.Y).sort_by(Axis.X)[0],
        ac_dc_box_mount.faces().sort_by(Axis.X).sort_by(Axis.Z)[-3].edges().filter_by(Axis.Y).sort_by(Axis.X)[0],
        ac_dc_box_mount.faces().sort_by(Axis.X).sort_by(Axis.Z)[-3].edges().filter_by(Axis.Y).sort_by(Axis.X)[-1],
        ac_dc_box_mount.faces().sort_by(Axis.X).sort_by(Axis.Z)[-4].edges().filter_by(Axis.Y).sort_by(Axis.X)[0],
        ac_dc_box_mount.faces().sort_by(Axis.X).sort_by(Axis.Z)[-4].edges().filter_by(Axis.Y).sort_by(Axis.X)[-1],
    ]
    fillet(top_side_edges, 8.0)

    bottom_plane = ac_dc_box_mount.faces().sort_by(Axis.Z)[0]
    with Locations(bottom_plane.offset(holder_height/2.0).center_location):
        add(attachment_part.part, rotation=(90, 0, 0))
    

# %%
show(ac_dc_box_mount.part)
# %%
export_stl(ac_dc_box_mount.part, "ac_dc_box_mount.stl")
# %%
