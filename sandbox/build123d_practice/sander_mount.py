# %%
from build123d import *
from ocp_vscode import show

# %%
slot_height = 40
mount_width = 150
mount_depth = 100
wall_thickness = 5
back_wall_thickness = 5
total_height = 200

# %%
with BuildPart() as mount:
    with BuildSketch():
        with BuildLine():
            def create_floor(n):
                return [
                    (back_wall_thickness + mount_depth, n * slot_height + n * wall_thickness),
                    (back_wall_thickness + mount_depth, n * slot_height + (n + 1) * wall_thickness),
                    (back_wall_thickness + mount_depth - 2, n * slot_height + (n + 1) * wall_thickness),
                    (back_wall_thickness + mount_depth - 3, n * slot_height + (n + 1) * wall_thickness - 1),
                    (back_wall_thickness, n * slot_height + (n + 1) * wall_thickness - 1),
                    (back_wall_thickness, (n + 1) * slot_height + (n + 1) * wall_thickness),
                ]

            Polyline(
                [
                    (0, 0),
                    *create_floor(0),
                    *create_floor(1),
                    *create_floor(2),
                    *create_floor(3),

                    (0, 4 * slot_height + 4 * wall_thickness),
                    (0, 0),
                ],
                close=True,
            )
        make_face()
    extrude(amount=sander_width / 2.0, both=False)

    back_faces = mount.faces().filter_by(Axis.X).group_by(Axis.X)[1].sort_by(Axis.Y)
    for face in back_faces:
        angle_edges = face.edges().filter_by(Axis.Z)
        fillet(angle_edges, radius=2.0)

    with Locations(back_faces[-1]):
        CounterSinkHole(3.0 / 2.0, 8.0 / 2.0, 5.0)
    
    with BuildSketch():
        with BuildLine():
            Polyline(
                [
                    (0, 0),
                    (back_wall_thickness + mount_depth, 0),
                    (back_wall_thickness + mount_depth, 4 * slot_height + 4 * wall_thickness),
                    (0, 4 * slot_height + 4 * wall_thickness),
                    (0, 0),
                ],
                close=True,
            )
        make_face()
    side_face = extrude(amount=-wall_thickness, both=False)

    corner_edge = side_face.edges().filter_by(Axis.Z).sort_by(Axis.Y).last
    fillet(corner_edge, radius=25)

    mirror(about=Plane.XY.offset(mount_width / 2.0))

# %%
show(mount)

# %%
export_stl(mount.part, "sander_mount.stl")
