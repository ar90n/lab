# %%
import cadquery as cq

from jupyter_cadquery import (
    versions,
    show, PartGroup, Part, 
    get_viewer, close_viewer, get_viewers, close_viewers, open_viewer, set_defaults, get_defaults, open_viewer,
    get_pick,
)

from jupyter_cadquery.replay import replay, enable_replay, disable_replay
''
enable_replay(False)

set_defaults(
    cad_width=640, 
    height=480, 
)

versions()

# %%
box = cq.Workplane('XY').box(1, 2, 3).edges().fillet(0.1)
box
# %%
cv = open_viewer("CadQuery", anchor="right")  # sets default viewer

# %%
box

# %%
show(box, viewer=None, up="Y")

# %%
cv2 = open_viewer("CadQuery2", anchor="split-bottom") 

# %%
cv.close()

# %%
close_viewers()

# %%
show?dd

# %%
show(
    box,
    viewer="CadQuery2",
    anchor="right",
    cad_width=640,
    tree_width=250,
    height=480,
    theme="light",
    pinning=False,
    
    angular_tolerance=0.1,
    deviation=1,
    edge_accuracy=0.01,
    default_color=(23, 176, 36),
    default_edge_color="#FF0000",
    optimal_bb=True,
    render_normals=True,
    render_edges=True,
    render_mates=True,
    mate_scale=2,

    control='trackball',
    axes=True,
    axes0=True,
    grid=[True, True,False],
    ticks=5,
    ortho=False,
    transparent=True,
    black_edges=False,
    position=(12.4, 7.7, -6.9),
    quaternion=(0.522, 0.611, 0.582, 0.120),
    target=(0.4, -1.6, -2.2),
    zoom=0.66,
    reset_camera=True,
    zoom_speed=3,
    pan_speed=3,
    rotate_speed=3,
    ambient_intensity=0.5,
    direct_intensity=0.05,
    show_parent=True,
    tools=True,
    timeit=False,
    js_debug=False
    
)
# %%
cv
# %%
bo
# %%
box
# %%
cv = open_viewer("Assembly", cad_width=720, height=540)

# %%
set_defaults(viewer="Assembly")

# %%
box1 = cq.Workplane('XY').box(10, 20, 30).edges(">X or <X").chamfer(2)
box1.name = "box1"

box2 = cq.Workplane('XY').box(8, 18, 28).edges(">X or <X").chamfer(2)
box2.name = "box2"

box3 = cq.Workplane('XY').transformed(offset=(0, 15, 7))\
    .box(30, 20, 6).edges(">Z").fillet(3)
box3.name = "box3"

box4 = box3.mirror("XY").translate((0, -5, 0))
box4.name = "box4"

box1 = box1\
    .cut(box2)\
    .cut(box3)\
    .cut(box4)
# %%
from jupyter_cadquery.ocp_utils import webcol_to_cq

a1 = (
    cq.Assembly(name="ensemble")
    .add(box1, name="red box", color=webcol_to_cq("#d7191c80")) # transparent alpha = 0x80/0xFF
    .add(box3, name="green box", color=webcol_to_cq("#abdda4"))
    .add(box4, name="blue box", color=cq.Color(43/255, 131/255, 186/255, 0.3)) # transparent, alpha = 0.3
)

cv = show(a1, axes=True, grid=[True, False, False], ortho=True, axes0=True, collapse=1)
# %%
show(a1, glass=True, transparent=True)

# %%
a1
# %%
box1
# %%
box
# %%
show(a1, viewer=None, up="Z")

# %%
cv
# %%
cv2
# %%
dir(cv2)
# %%
import cadquery as cq
from jupyter_cadquery import show, PartGroup, Part, Faces, Edges, web_color, set_defaults, open_viewer, close_viewer, close_viewers
from cadquery_massembly import Mate, MAssembly, relocate

cv = open_viewer("Linkage", cad_width=640, height=500, theme="light")

# bypass "clean" to avoid errors OCP kernel error
cq.occ_impl.shapes.Shape.clean = lambda x: x
# %%
import cadquery as cq
from jupyter_cadquery import show, PartGroup, Part, Faces, Edges, web_color, set_defaults, open_viewer, close_viewer, close_viewers
from cadquery_massembly import Mate, MAssembly, relocate

cv = open_viewer("Linkage", cad_width=640, height=500, theme="light")

# bypass "clean" to avoid errors OCP kernel error
cq.occ_impl.shapes.Shape.clean = lambda x: x

# %%
set_defaults(axes=False, axes0=True, mate_scale=2)

# %%
import math
import numpy as np

Vec = lambda x,y: np.array((x, y))

def intersect(p0, r0, p1, r1):
    """
    Bourke's algorithm (http://paulbourke.net/geometry/circlesphere)
    to find intersect points of circle0 (p0, r0) and circle1 (p1, r1)
    """
    p10 = p1 - p0
    d = np.linalg.norm(p10)
    if (d > r0 + r1) or (d < abs(r1 - r0)) or ((d == 0) and (r0 == r1)):
        return None
    
    a = (r0**2 - r1**2 + d**2) / (2 * d)
    h = np.sqrt(r0**2 - a**2)
    p2 = p0 + (a / d) * p10
    r = Vec(-p10[1], p10[0]) * (h / d)

    return (p2 - r, p2 + r)


def link_loc(name, joints, links):
    p1_index, p2_index = name.split("_")[1:]
    p1 = joints[int(p1_index)]
    p2 = joints[int(p2_index)]
    a = math.degrees(math.atan2(p1[1] - p2[1], p1[0] - p2[0]))
    return (np.array((links[name]["lev"], *p1)), a)

    
def linkage(alpha, x, y, links):
    """For a given angle return the 2d location of each joint"""
    p0 = Vec(0, 0)
    p1 = Vec(x, y)
    p2 = p1 + links["link_1_2"]["len"] * Vec(np.cos(np.deg2rad(alpha)), np.sin(np.deg2rad(alpha)))
    p3 = intersect(p0, links["link_0_3"]["len"], p2, links["link_2_3"]["len"])[1]
    p4 = intersect(p0, links["link_4_0"]["len"], p3, links["link_3_4"]["len"])[1]
    p5 = intersect(p0, links["link_0_5"]["len"], p2, links["link_2_5"]["len"])[0]
    p6 = intersect(p4, links["link_4_6"]["len"], p5, links["link_5_6"]["len"])[0]
    p7 = intersect(p5, links["link_7_5"]["len"], p6, links["link_6_7"]["len"])[1]
    return (p0, p1, p2, p3, p4, p5, p6, p7)

height = 2
x = 38.0
y =  7.8

links = {}
links["link_1_2"] = {"len": 15.0, "lev": 3 * height, "col": "DarkBlue"}
links["link_2_3"] = {"len": 50.0, "lev": 4 * height, "col": "DarkGreen"}
links["link_3_4"] = {"len": 55.8, "lev": 3 * height, "col": "Red"}
links["link_4_0"] = {"len": 40.1, "lev": 1 * height, "col": "Red"}
links["link_0_3"] = {"len": 41.5, "lev": 2 * height, "col": "Red"}
links["link_4_6"] = {"len": 39.4, "lev": 2 * height, "col": "Purple"}
links["link_0_5"] = {"len": 39.3, "lev": 3 * height, "col": "OliveDrab"}
links["link_2_5"] = {"len": 61.9, "lev": 1 * height, "col": "Orange"}
links["link_5_6"] = {"len": 36.7, "lev": 0 * height, "col": "RoyalBlue"}
links["link_6_7"] = {"len": 65.7, "lev": 1 * height, "col": "RoyalBlue"}
links["link_7_5"] = {"len": 49.0, "lev": 2 * height, "col": "RoyalBlue"}

link_list = list(links.keys())
# %%
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
%matplotlib inline

def c(a,b):
    return links[f"link_{a}_{b}"]["col"].replace("Blue4", "blue")

def plot(ax, joints):
    p0, p1, p2, p3, p4, p5, p6, p7 = joints
    lines = (
        (p1, p2, c(1,2)), (p2, p5, c(2,5)), (p2, p3, c(2,3)), (p0, p3, c(0,3)), (p4, p0, c(4,0)), (p3, p4, c(3,4)), 
        (p4, p6, c(4,6)), (p0, p5, c(0,5)), (p5, p6, c(5,6)), (p7, p5, c(7,5)), (p6, p7, c(6,7))
    )
    ax.scatter((p0[0], p1[0]), (p0[1], p1[1]))
    for a, b, col in lines:
        ax.plot((a[0], b[0]), (a[1], b[1]), color=col)

fig = plt.figure(constrained_layout=True)
fig.set_size_inches(15, 5)
spec2 = gridspec.GridSpec(ncols=6, nrows=2, figure=fig)

for i, alpha in enumerate(range(0,360, 30)):
    joints = linkage(alpha, x, y, links)
    ax = fig.add_subplot(spec2[i//6, i%6])
    ax.set_xlim(-70, 60)
    ax.set_ylim(-90, 50)
    plot(ax, joints)
# %%
def make_link(length, width=2, height=1):
    link = (
        cq.Workplane("YZ").rect(length + 4, width + 2)
          .pushPoints(((-length/2, 0), (length/2, 0))).circle(1)
          .extrude(height).edges("|X").fillet(1.99)
    )
    link.faces(">X").wires(cq.NearestToPointSelector((0, length/2))).tag("mate")
    return link

parts = {name: make_link(links[name]["len"], height=(2 * height if name == "link_1_2" else height)) 
         for name in link_list}
# %%
def create_leg(x, y):
    L = lambda *args: cq.Location(cq.Vector(*args))
    C = lambda name: web_color(name)

    leg = MAssembly(cq.Workplane("YZ").polyline([(0,0), (x, 0),(x,y)]), name="base", color=C("Gray"))
    for i, name in enumerate(link_list):
        leg.add(parts[name], name=name, color=C(links[name]["col"]), loc=L(0, 0, i*10 - 50))
    return leg

leg = create_leg(x, y)
d = show(leg, axes=False)
# %%
leg = create_leg(x, y)

for name in link_list:
    leg.mate(f"{name}?mate", name=name, origin=True)
    
d = show(leg, render_mates=True, axes=False)
# %%
leg.relocate()
 
d = show(leg, render_mates=True)
# %%
alpha = 0
joints = linkage(alpha, x, y, links)

for name in link_list:
    v, a = link_loc(name, joints, links)
    abs_loc = cq.Location(cq.Workplane("YZ").plane.rotated((0,0,a)), cq.Vector(*v))  # calculate the absolute location ...
    loc = abs_loc * leg.mates[name].mate.loc.inverse                                 # ... and center the mate of the link first
    leg.assemble(name, loc)

cv = show(leg, render_mates=True)
# %%
from jupyter_cadquery.animation import Animation

alphas = {name: [] for name in link_list}
positions = {name: [] for name in link_list}

for alpha in range(0, -375, -15):
    for name in link_list:
        p, a = link_loc(name, linkage(alpha, x, y, links), links)        
        alphas[name].append(a)
        positions[name].append(p)

time = np.linspace(0, 4, 25)

animation = Animation(cv)

for name in link_list:
    animation.add_track(f"/base/{name}", "t",  time, [(p - positions[name][0]).tolist() for p in positions[name]])     
    animation.add_track(f"/base/{name}", "rz", time, [a - alphas[name][0] for a in alphas[name]])       

animation.animate(2)
# %%
d
# %%
show(leg, viewer=None, axes=False)
# %%
leg = create_leg(x, y)

for name in link_list:
    leg.mate(f"{name}?mate", name=name, origin=True)
    
d = show(leg, viewer=None, render_mates=True, axes=False)
# %%
leg.relocate()
 
d = show(leg, viewer=None, render_mates=True)
# %%

alpha = 0
joints = linkage(alpha, x, y, links)

for name in link_list:
    v, a = link_loc(name, joints, links)
    abs_loc = cq.Location(cq.Workplane("YZ").plane.rotated((0,0,a)), cq.Vector(*v))  # calculate the absolute location ...
    loc = abs_loc * leg.mates[name].mate.loc.inverse                                 # ... and center the mate of the link first
    leg.assemble(name, loc)

cv = show(leg, viewer=None, render_mates=True)
# %%
show(leg)
# %%
from jupyter_cadquery.viewer.client import show, show_object

# %%
show(leg)
# %%
