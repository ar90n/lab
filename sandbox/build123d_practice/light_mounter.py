# %%
from build123d import *
from ocp_vscode import show, show_object
# %%
pw = 91.5
ph = 91.5
pww = 5
wd = 5
ww = 2

w = pw + 2 * ww
h = ph + 2 * ww
d = 55 + wd
bd = 1

shh = 30
shw = 60
shcd = 15

w2 = w / 2
h2 = h / 2
with BuildPart() as body:
    Box(w, h, d,)
    with BuildSketch(body.faces().sort_by(Axis.Z).first):
        Rectangle(pw, ph)
    extrude(amount=-wd, mode=Mode.SUBTRACT)

    with BuildSketch(body.faces().filter_by(Axis.Z).sort_by(Axis.Z)[1]):
        Rectangle(pw - 2 * ww, ph - 2 * ww)
    extrude(amount=-(d - wd - bd), mode=Mode.SUBTRACT)

    with BuildSketch(body.faces().filter_by(Axis.Z).sort_by(Axis.Z)[2]):
        with GridLocations((pw - 2 * pww) / 2.0, (ph - 2 * pww) / 2.0, 2, 2):
            Rectangle((pw - 2 * pww) / 2.0 - pww, (ph - 2 * pww) / 2.0 - pww)
        Circle(25.0,mode=Mode.SUBTRACT)
        Circle(20.0)
    extrude(amount=-bd, mode=Mode.SUBTRACT)

    with BuildSketch(body.faces().filter_by(Axis.X).sort_by(Axis.X)[2]):
        with Locations((0, 5)):
            RectangleRounded(shw, shh, shh/2.0 - 1e-12)
    extrude(amount=-h, mode=Mode.SUBTRACT)

    with BuildSketch(body.faces().filter_by(Axis.X).sort_by(Axis.X)[-3]):
        with Locations((0, -5)):
            RectangleRounded(shw, shh, shh/2.0 - 1e-12)
    extrude(amount=-h, mode=Mode.SUBTRACT)
show(body)
# %%
body.part.export_stl("light_mounter.stl")
# %%
