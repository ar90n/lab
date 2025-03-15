# %%
import math
from build123d import *
from ocp_vscode import show
# %%
box_width = 150.0
box_height = 100.0
box_depth = 40.0
wall_thickness = 2.54

dc_dc_con_width = 67.4
dc_dc_con_height = 58.3
dc_dc_con_hole_interval = (47.8 + 61.8) / 2
dc_dc_con_hole_diameter = (61.8 - 47.8) / 2
dc_dc_con_spacer_depth = 3.0
dc_dc_con_spacer_diameter = dc_dc_con_hole_diameter * 1.5

usb_socket_width = 21.5
usb_socket_height = 10.0

hook_hole_interval = 77.0
hook_hole_radius = 3.2
hook_hole_mount_width = 12.0
hook_hole_mount_height = 14.0
hook_hole_mount_depth = 14.0

pcb_width = 20.5
pcb_height = 80.0

fuse_radius = 7.0 / 2.0
fuse_mount_width = (fuse_radius + 0.6) * 2.0
fuse_mount_height = 10.0
fuse_mount_depth = 5.0
# %%
with BuildPart() as dc_dc_mount:
    spacer = Cylinder(dc_dc_con_spacer_diameter / 2.0, dc_dc_con_spacer_depth, align=[Align.CENTER, Align.CENTER, Align.MIN])
    pillar = Cylinder(dc_dc_con_hole_diameter / 2.0 * 0.9, dc_dc_con_spacer_depth + 3.3, align=[Align.CENTER, Align.CENTER, Align.MIN])
    pillar_top_face = pillar.faces().sort_by(Axis.Z)[-1]
    with BuildSketch(pillar_top_face):
        Circle(dc_dc_con_hole_diameter / 2.0 * 0.9 + 0.4)
    with BuildSketch(Plane(pillar_top_face).offset(3.3)):
        Circle(dc_dc_con_hole_diameter / 2.0 * 0.9)
    loft()

    spacer_top_face = spacer.faces().sort_by(Axis.Z)[-1]
    with BuildSketch(spacer_top_face):
        Rectangle(1.0, dc_dc_con_spacer_diameter)
        Rectangle(dc_dc_con_spacer_diameter, 1.0)
    extrude(amount=dc_dc_con_spacer_depth + 4.0, mode=Mode.SUBTRACT)

# %%
with BuildPart() as hook_mount:
    with BuildSketch():
        with BuildLine():
            l1 = Polyline([
                (-hook_hole_mount_width / 2.0, hook_hole_mount_height - hook_hole_mount_width/2.0),
                (-hook_hole_mount_width / 2.0, 0),
                ( hook_hole_mount_width / 2.0, 0),
                ( hook_hole_mount_width / 2.0, hook_hole_mount_height - hook_hole_mount_width/2.0),
                (-hook_hole_mount_width / 2.0, hook_hole_mount_height - hook_hole_mount_width/2.0),
            ])
        make_face()
        with Locations((0, hook_hole_mount_height - hook_hole_mount_width / 2.0)):
            Circle(hook_hole_mount_width / 2.0)
    extrude(amount=hook_hole_mount_depth * 1.5)
    with BuildSketch():
        with Locations((0, hook_hole_mount_height - hook_hole_mount_width / 2.0)):
            Circle(hook_hole_radius)
    extrude(amount=hook_hole_mount_depth * 1.5, mode=Mode.SUBTRACT)

    split(bisect_by=Plane.XY.rotated((45, 0, 0)))

show(hook_mount.part)


# %%
with BuildPart() as pcb_mount:
    with BuildSketch():
        with BuildLine():
            Polyline([
                (-2.0, 0.0),
                ( 2.0, 0.0),
                ( 2.0, 2.0 + 2.0 + 1.0 + 0.4),
                ( 0.5, 2.0 + 2.0 + 0.5 + 0.4),
                ( 0.5, 2.0 + 2.0 + 0.0 + 0.4),
                ( 1.0, 2.0 + 2.0 + 0.0 + 0.4),
                ( 1.0, 2.0 + 0.0 + 0.0 + 0.4),
                (-2.0, 2.0 + 0.0 + 0.0 + 0.4),
                (-2.0, 0.0),
            ])
        make_face()
    extrude(amount=pcb_width * 0.48, both=True)
show(pcb_mount.part)
# %%
with BuildPart() as fuse_mount:
    with BuildSketch():
        Rectangle(fuse_mount_width, fuse_mount_height, align=[Align.CENTER, Align.MIN])
        with Locations((0, fuse_mount_height - fuse_radius * 0.4)):
            Circle(fuse_radius, mode=Mode.SUBTRACT)
    extrude(amount=fuse_mount_depth)
show(fuse_mount.part)
# %%
with BuildPart() as bottom_part:
    with BuildSketch():
        Rectangle(box_width, box_height)
    extrude(amount=box_depth)

    inner_width = box_width - 2 * wall_thickness
    inner_height = box_height - 2 * wall_thickness
    inner_depth = box_depth - wall_thickness
    top_face = bottom_part.faces().sort_by(Axis.Z)[-1]
    with BuildSketch(top_face):
        Rectangle(inner_width, inner_height)
    extrude(amount=-inner_depth, mode=Mode.SUBTRACT)

    right_face = bottom_part.faces().sort_by(Axis.X)[-1]
    with BuildSketch(right_face):
        with Locations((0, box_depth / 2.0)):
            Rectangle(inner_height, 1, align=[Align.CENTER, Align.MAX])
    extrude(amount=-wall_thickness, mode=Mode.SUBTRACT)

    inner_floor_face = bottom_part.faces().sort_by(Axis.Z)[1]
    kirikomi = wall_thickness * (1 / 2) + 0.25
    with BuildSketch(inner_floor_face.offset(inner_depth - 1.0 - kirikomi)):
        with Locations((5, 0)):
            Rectangle(box_width - 2 * (wall_thickness - kirikomi) + 10, box_height - 2 * (wall_thickness - kirikomi))
    with BuildSketch(inner_floor_face.offset(inner_depth - 1.0)):
        with Locations((5, 0)):
            Rectangle(inner_width + 10, inner_height)
    loft(mode=Mode.SUBTRACT)

    with Locations(inner_floor_face.center_location):
        with Locations((-25, 5)):
            hole_locs = [
                (dc_dc_con_hole_interval / 2, 0),
                (-dc_dc_con_hole_interval / 2, 0),
            ]
            for loc in hole_locs:
                with Locations(loc):
                    add(dc_dc_mount)
        with Locations((55, 38)):
            add(pcb_mount, rotation=(90, 90, 0))
        with Locations((55, 38 - pcb_height + 1.7)):
            add(pcb_mount, rotation=(90, -90, 0))
        with Locations((30, 15)):
            add(fuse_mount, rotation=(90, 0, 0))
        with Locations((30, -22)):
            add(fuse_mount, rotation=(90, 0, 0))


    left_face = bottom_part.faces().sort_by(Axis.X)[0]
    with BuildSketch(left_face):
        with Locations((box_height / 2 - 15, 0)):
            RectangleRounded(6.5, 3.2, 1.0)
    extrude(amount=-wall_thickness, mode=Mode.SUBTRACT)

    down_face = bottom_part.faces().sort_by(Axis.Y)[0]
    with BuildSketch(down_face):
        with GridLocations(box_width / 4, 0, 4, 1):
            Rectangle(usb_socket_width, usb_socket_height)
    extrude(amount=-wall_thickness, mode=Mode.SUBTRACT)

    top_face = bottom_part.faces().sort_by(Axis.Y)[-1]
    with Locations(top_face):
        with Locations((-hook_hole_interval / 2, box_depth / 2.0 - 5.0)):
            add(hook_mount, rotation=(90, 0, 0))
        with Locations((hook_hole_interval / 2, box_depth / 2.0 - 5.0)):
            add(hook_mount, rotation=(90, 0, 0))

# %%
show(bottom_part)
# %%
export_stl(bottom_part.part, 'dc_dc_box_bottom.stl')
# %%

# %%
clearance = 0.2
with BuildPart() as top_part:
    kirikomi = wall_thickness * (1 / 2) + 0.25
    with BuildSketch(inner_floor_face.offset(inner_depth - 1.0 - kirikomi)):
        with Locations((box_width / 2.0, 0)):
            Rectangle(box_width - (wall_thickness - kirikomi) - clearance, box_height - 2 * (wall_thickness - kirikomi) - 2 * clearance, align=[Align.MAX, Align.CENTER])    
    with BuildSketch(inner_floor_face.offset(inner_depth - 1.0)):
        with Locations((box_width / 2.0, 0)):
            Rectangle(inner_width + wall_thickness - clearance, inner_height - 2 * clearance, align=[Align.MAX, Align.CENTER])
    loft()

    top_face = top_part.faces().sort_by(Axis.Z)[-1]
    with Locations(top_face.center_location):
        Box(inner_width + wall_thickness - clearance, inner_height - 2 * clearance, 1.0, align=[Align.CENTER, Align.CENTER, Align.MIN])

    top_face = top_part.faces().sort_by(Axis.Z)[-1]
    with BuildSketch(top_face) :
        with HexLocations(10, 10, 5):
            RegularPolygon(radius=10.0, side_count=6)
        Rectangle(inner_width - 2 * 5.0, inner_height - 2 * 5.0, mode=Mode.INTERSECT)
    extrude(amount=-wall_thickness, mode=Mode.SUBTRACT)
    

# %%
show(top_part.part + bottom_part.part)
# %%
export_stl(top_part.part, 'dc_dc_box_top.stl')
# %%
