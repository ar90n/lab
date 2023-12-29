# %%
from build123d import *
from ocp_vscode import show
# %%
clearance = 0.15
thickness = 2.0 
body_depth = 25.0 + 2 * clearance
body_width = 51.2 + 2 * clearance
body_height = 100.0
holder_depth = body_depth + 2 * thickness
holder_width = body_width + 2 * thickness
holder_height = body_height + thickness
right_side_hole_interval = 63.5
# %%
body_mid_width = body_width - body_depth
holder_mid_width = holder_width - holder_depth

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
            CounterBoreHole(3.0 / 2.0, 5.7 / 2.0, 4.0)
    return holder

# %%
with BuildPart() as battery_body:
    with BuildSketch() as s:
        Rectangle(body_mid_width / 2.0, body_depth, align=[Align.MIN, Align.CENTER])
        with Locations((body_mid_width / 2.0, 0)):
            Circle(body_depth / 2.0)
        mirror(about=Plane.YZ)
    extrude(amount=body_height)
    
show(battery_body)
# %%
with     BuildPart() as battery_holder:
    with BuildSketch() as s:
        Rectangle(holder_mid_width / 2.0, holder_depth, align=[Align.MIN, Align.CENTER])
        Rectangle(holder_width / 2.0, holder_depth / 2.0, align=[Align.MAX, Align.MAX])
        with Locations((holder_mid_width / 2.0, 0)):
            Circle(holder_depth / 2.0)
        mirror(about=Plane.YZ)
    extrude(amount=holder_height)

    topf = battery_holder.faces().sort_by(Axis.Y).last
    mod = 1.3
    ww = 65.5 + mod
    with Locations(topf):
        for loc in [(25.8 / 2.0, ww / 2.0), (-25.8 / 2.0, ww / 2.0), (25.8 / 2.0, -ww / 2.0), (-25.8 / 2.0, -ww / 2.0)]:
            with Locations(loc):
                p = Cone(9, 4.0, 6.0)
            with BuildSketch(p.faces().sort_by(Axis.Y).last):
                RegularPolygon(radius=3.0, side_count=6)
            extrude(amount=-3.2, mode=Mode.SUBTRACT)

    with Locations((holder_width / 2.0, -holder_depth / 4.0, (holder_height + right_side_hole_interval) / 2.0)):
        with Locations(Rotation(90, 0, -90)):
            holder = Holder(20.0, 6.0, holder_depth / 4.0)
            add(holder)

    with Locations((holder_width / 2.0, -holder_depth / 4.0, (holder_height - right_side_hole_interval) / 2.0)):
        with Locations(Rotation(90, 0, -90)):
            holder = Holder(20.0, 6.0, holder_depth / 4.0)
            add(holder)

    with Locations((-holder_width / 2.0, -holder_depth / 4.0, holder_height / 2.0)):
        with Locations(Rotation(-90, 180, -90)):
            holder = Holder(20.0, 6.0, holder_depth / 4.0)
            add(holder)

    add(battery_body, mode=Mode.SUBTRACT)

    with Locations(battery_holder.faces().sort_by(Axis.Z).last):
        Hole(10.0)

show(battery_holder.part)
# %%
battery_holder.part.export_stl("battery_holder.stl")