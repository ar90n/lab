# %%
from build123d import *
from ocp_vscode import show

# %%
with BuildPart() as wheel:
    Cylinder(5, 2)
    with BuildSketch(wheel.faces().sort_by(Axis.Z)[-1]) as upper_face:
        Circle(2)
    with BuildSketch(upper_face.faces().first.offset(1 + 2)):
        Circle(0.01)
    loft()
    mirror(about=Plane.XY)
    RigidJoint(
        "center",
        wheel.part,
        Location((0, 0, 0), (90, 0, 0)),
    )
wheels = [wheel.part.copy() for _ in range(4)]

# %%
with BuildPart() as wheel_clearance:
    Box(11, 11, 2 + 2 * 0.5)
    with BuildSketch(wheel_clearance.faces().sort_by(Axis.Z)[-1]) as upper_face:
        Circle(2.5)
    with BuildSketch(upper_face.faces().first.offset(1 + 2 + 0.5)):
        Circle(0.11)
    loft()
    mirror(about=Plane.XY)
    RigidJoint(
        "center",
        wheel_clearance.part,
        Location((0, 0, 0), (90, 0, 0)),
    )

# %%
body = Box(20, 15, 5)
plane = Plane(body.faces().sort_by(Axis.Z)[-1])
wheel_regions = []
wheel_joints = {}
for i, loc in enumerate(GridLocations(15, 7.5, 2, 2)):
    pos = plane * loc
    wheel_joints[f"wheel_{i}"] = RigidJoint(
        f"wheel_{i}", to_part=body, joint_location=loc * Location((0, 0, 0))
    )
    body -= wheel_clearance.part.moved(loc * Location((0, 0, 0), (90, 0, 0)))

for k, v in wheel_joints.items():
    body.joints[k] = v
# %%
for j0, j1 in zip(wheel_joints.values(), wheels):
    j0.connect_to(j1.joints["center"])
# %%
show(body + wheels[0] + wheels[1] + wheels[2] + wheels[3])
# %%
whole_parts = body + wheels[0] + wheels[1] + wheels[2] + wheels[3]
whole_parts.export_stl("plain_car.stl")
# %%
