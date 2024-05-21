# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
pipe_diameter = 8.0
pipe_clearance = 0.25
# %%
with BuildPart() as board:
    with BuildSketch():
        Rectangle(15, 15)
    extrude(amount=8)

    #with Locations(board.faces().sort_by(Axis.Z).first.center_location):
    #    Hole(pipe_diameter / 2.0)

    #with BuildSketch(board.faces().sort_by(Axis.Z).first.center_location):
    #    Circle(pipe_diameter / 2.0)
    #with BuildSketch(board.faces().sort_by(Axis.Z).last.center_location):
    #    Circle((pipe_diameter - pipe_clearance) / 2.0)
    #loft(mode=Mode.SUBTRACT)

    with BuildSketch(board.faces().sort_by(Axis.Z).first.center_location):
        Circle((pipe_diameter + 2 * pipe_clearance) / 2.0)
    with BuildSketch(board.faces().sort_by(Axis.Z).last.center_location):
        Circle((pipe_diameter + pipe_clearance ) / 2.0)
    loft(mode=Mode.SUBTRACT)


show(board)
# %%
board.part.export_stl("board_4.stl")
# %%
