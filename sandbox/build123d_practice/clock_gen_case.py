# %%
from build123d import *
from ocp_vscode import show
# %%
gen_width = 71
gen_height = 39
gen_panel_width_margin = 4
gen_panel_height_margin = 1
bnc_hole = 10.5
type_c_hole_height = 4
type_c_hole_width = 10
type_c_hole_margin = 7
wall_thickness = 3.0

case_width = gen_width + gen_panel_width_margin * 2 + 5.0 * 2 + 2 * wall_thickness
case_height = gen_height + gen_panel_height_margin * 2 + 3.0 * 2 + 2 * wall_thickness
case_depth = 38.0 + 2 * wall_thickness

# %%
with BuildPart() as bottom_panel:
    clearanance = 0.5
    with BuildSketch(Plane.YZ):
        with BuildLine():
            edge_size = wall_thickness - 1.5
            Polyline([
                (0,0),
                (-clearanance + case_height / 2 - wall_thickness, 0),
                (-clearanance + case_height / 2 - wall_thickness, wall_thickness - edge_size),
                (-clearanance + case_height / 2- wall_thickness + edge_size, wall_thickness),
                (-(case_height / 2 - wall_thickness + edge_size), wall_thickness),
                (-(case_height / 2 -wall_thickness), wall_thickness - edge_size),
                (-(case_height / 2 -wall_thickness), 0),
                (0,0),
            ], close=True)
        make_face()    
    extrude(amount=case_width - wall_thickness, both=False)

show(bottom_panel)
# %%
with BuildPart() as clock_gen_case:
    Box(case_width, case_height, case_depth, align=(Align.MIN, Align.CENTER, Align.MIN))
    offset(amount=-wall_thickness, openings=clock_gen_case.faces().sort_by(Axis.Z)[0])

    inner_top_face = clock_gen_case.faces().sort_by(Axis.Z)[-2]
    with BuildSketch(inner_top_face):
        Rectangle(
            gen_width + gen_panel_width_margin * 2,
            gen_height + gen_panel_height_margin * 2,
        )
    extrude(amount=-(wall_thickness - 0.8), mode=Mode.SUBTRACT)

    outer_top_face = clock_gen_case.faces().sort_by(Axis.Z)[-1]
    with BuildSketch(outer_top_face):
        Rectangle(gen_width, gen_height)
    extrude(amount=-wall_thickness, mode=Mode.SUBTRACT)

    input_side_face = clock_gen_case.faces().sort_by(Axis.X)[-1]
    with BuildSketch(input_side_face):
        with Locations((-((gen_height - wall_thickness) / 2) * 0.8 + type_c_hole_margin, 0, 0)):
            RectangleRounded(type_c_hole_height, type_c_hole_width, (type_c_hole_height / 2) * 0.99)
    extrude(amount=-wall_thickness, mode=Mode.SUBTRACT)

    output_side_face = clock_gen_case.faces().sort_by(Axis.X)[0]
    with BuildSketch(output_side_face):
        Circle(bnc_hole / 2)
    extrude(amount=-wall_thickness, mode=Mode.SUBTRACT)
    add(bottom_panel, mode=Mode.SUBTRACT)

# %%
show(clock_gen_case)
# %%
export_stl(clock_gen_case.part, "clock_gen_case.stl")
export_stl(bottom_panel.part, "clock_gen_case_bottom_panel.stl")


# %%
