
# %%
import math
from build123d import *
from ocp_vscode import show
# %%
plate_width = 135.0
plate_height = 75.0
plate_depth = 3.0

plug_margin = 1.5
plug1_front_widht = 23.0
plug1_front_height = 16.0
plug1_widht = 20.0
plug1_height = 12.0

plug2_front_widht = 31.0
plug2_front_height = 24.0
plug2_widht = 27.5
plug2_height = 20.0

box_width = 150.0
box_height = 40.0
box_wall_thickness = 2.0 + 0.3

current_sensor_width = 25.0
current_sensor_height = 44.0
current_sensor_depth = 26.0
current_sensor_mount_thickness = 2.0

# %%
with BuildPart() as plate:
    with BuildSketch(Location(Plane.ZX, (0, -plate_height / 5.0))):
        with BuildLine(Location((plate_depth, current_sensor_width / 2.0 + current_sensor_mount_thickness))):
            l1 = Polyline([
                (0, 0),
                (0, current_sensor_mount_thickness),
                (current_sensor_depth + current_sensor_mount_thickness, 0),
                (current_sensor_depth + current_sensor_mount_thickness,-2 * current_sensor_mount_thickness),
                (current_sensor_depth,-2 * current_sensor_mount_thickness),
                (current_sensor_depth - current_sensor_mount_thickness, -current_sensor_mount_thickness),
                (0 ,-current_sensor_mount_thickness),
            ])
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
    side_mounts = extrude(amount=(current_sensor_width /2.0 - 1.0))

    with BuildSketch(Plane.YZ):
        with BuildLine(Location((-current_sensor_height / 2.0, plate_depth))):
            l1 = Polyline([
                (0, 0),
                (0, 0.9 * current_sensor_depth),
                (-current_sensor_mount_thickness, 0.9 * current_sensor_depth),
                (-2 *current_sensor_mount_thickness, 0),
            ])
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
    bottom_mounts = extrude(amount=7)

    mounts = bottom_mounts + side_mounts
    mirror(mounts, Plane.YZ)

    with BuildSketch():
        Rectangle(plate_width, plate_height)
    bb = extrude(amount=plate_depth)
    fillet(bb.edges().filter_by(Axis.Z), 5.0)

    with GridLocations(x_spacing=(plate_width - 15.0), y_spacing=(plate_height - 15.0), x_count=2, y_count=2):
        Cone(5, 2.5, 4.0, mode=Mode.SUBTRACT)
        Cylinder(2.5, plate_depth, mode=Mode.SUBTRACT, align=[Align.CENTER, Align.CENTER, Align.MIN])

    locs = [loc for loc in GridLocations(x_spacing=(plate_width / 2.0), y_spacing=plate_height, x_count=2, y_count=1)]
    with Locations(locs[0]):
        Box(plug1_widht, plug1_height, plate_depth,align=[Align.CENTER, Align.CENTER, Align.MIN], mode=Mode.SUBTRACT)
        Box(plug1_front_widht, plug1_front_height, (plate_depth - plug_margin), align=[Align.CENTER, Align.CENTER, Align.MIN], mode=Mode.SUBTRACT)
    with Locations(locs[1]):
        Box(plug2_widht, plug2_height, plate_depth,align=[Align.CENTER, Align.CENTER, Align.MIN], mode=Mode.SUBTRACT)
        Box(plug2_front_widht, plug2_front_height, (plate_depth - plug_margin), align=[Align.CENTER, Align.CENTER, Align.MIN], mode=Mode.SUBTRACT)


    with Locations((0, 1.0, plate_depth)):
        Box(current_sensor_width, 4.0, 1.8, align=[Align.CENTER, Align.CENTER, Align.MAX], mode=Mode.SUBTRACT)

    #with Locations((0, 0, plate_depth)):
    #    sens = Box(current_sensor_width, current_sensor_height, current_sensor_depth, align=[Align.CENTER, Align.CENTER, Align.MIN])
    #fillet(sens.edges().filter_by(Axis.Y), 1.0)


# %%
show(plate)
# %%
plate.part.export_stl("plate.stl")

# %%
