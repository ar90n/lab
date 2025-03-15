# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
inner_base_width = 34.0
inner_base_height = 20.7
inner_base_depth = 3.8
inner_hook_depth = 19.5
inner_hook_width = 15.0
inner_hook_tsume_length = 1.2
pillar_diameter = 5.4
pillar_length = 5.1
hole_interval = 25.4
wall_thinkness = 2.0
clearance = 0.5
# %%
outer_base_width = inner_base_width + 2 * wall_thinkness
outer_base_height = inner_base_height + 2 * wall_thinkness
outer_base_depth = inner_base_depth + wall_thinkness

# %%
with BuildPart() as pillar_part:
    pillar = Cylinder(pillar_diameter / 2.0 * 0.90, pillar_length + wall_thinkness, align=[Align.CENTER, Align.CENTER, Align.MAX])
    pillar_top_face = pillar.faces().sort_by(Axis.Z)[0]
    with BuildSketch(pillar_top_face):
        Circle(pillar_diameter / 2.0 * 0.90 + 0.2)
    with BuildSketch(Plane(pillar_top_face).offset(3.3)):
        Circle(pillar_diameter / 2.0 * 0.90)
    loft()

    spacer_top_face = pillar_part.faces().sort_by(Axis.Z)[0]
    with BuildSketch(spacer_top_face):
        Rectangle(0.95, pillar_diameter + 1.5)
        Rectangle(pillar_diameter + 1.5, 0.95)
    extrude(amount=-(pillar_length * 0.6 + 3.3), mode=Mode.SUBTRACT)
# %%
with BuildPart() as part:
    with BuildSketch():
        Rectangle(outer_base_width + clearance, outer_base_height / 2.0, align=[Align.CENTER, Align.MIN])
        Rectangle(inner_base_width + clearance, inner_base_height / 2.0, align=[Align.CENTER, Align.MIN], mode=Mode.SUBTRACT)
    extrude(amount=inner_base_depth)

    with BuildSketch(Plane.YZ):
        with BuildLine(Location(((outer_base_height - wall_thinkness) / 2.0, inner_base_depth, 0))):
            Polyline([
                (0.0, 0.0),
                (wall_thinkness / 2.0, 0.0),
                (wall_thinkness / 2.0, inner_hook_depth + wall_thinkness - inner_base_depth),
                (-wall_thinkness / 2.0, inner_hook_depth + wall_thinkness - inner_base_depth),
                (-wall_thinkness / 2.0 - inner_hook_tsume_length, inner_hook_depth - inner_base_depth),
                (-wall_thinkness / 2.0, inner_hook_depth - inner_base_depth),
                (-wall_thinkness / 2.0, 0.0),
                (0.0, 0.0),
            ])
        make_face()
    extrude(amount=inner_hook_width / 2.0 + clearance, both=True)

    with BuildSketch():
        Rectangle(outer_base_width, outer_base_height / 2.0, align=[Align.CENTER, Align.MIN])
    extrude(amount=-wall_thinkness)

    mirror(about=Plane.XZ)

    bottom_face = part.faces().sort_by(Axis.Z).first
    with Locations(bottom_face.offset(-wall_thinkness + 0.25)):
        for xc, yc in [(-1, -1), (-1, 1), (1, -1), (1, 1)]:
            loc = (xc * hole_interval / 2.0, yc * hole_interval / 2.0)
            with Locations(Location(loc, (0, 180, 0))):
                add(pillar_part)
    

# %%
show(part.part)
# %%
export_stl(part.part, "ether_con_wall_mount.stl")
# %%
