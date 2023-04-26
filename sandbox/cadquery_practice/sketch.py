# %%
import cadquery as cq
from jupyter_cadquery.viewer.client import show, show_object
# %%
result = (
    cq.Sketch()
    .trapezoid(4, 3, 90)
    .vertices()
    .circle(.5, mode='s')
    .reset()
    .vertices()
    .fillet(0.25)
    .reset()
    .rarray(.6, 1, 5, 1).slot(1.5, 0.4, mode='s', angle=90)
)
show(result)
# %%
result = (
    cq.Sketch()
    .rect(1, 2, mode='c', tag='base')
    .vertices(tag='base')
    .circle(.7)
    .reset()
    .edges('|Y', tag='base')
    .ellipse(1.2, 1, mode='i')
    .reset()
    .rect(2, 2, mode='i')
    .clean()
)
show(result)
# %%
result = (
    cq.Sketch()
    .segment((0.,0), (0., 2.))
    .segment((2.,0))
    .close()
    .arc((.6, .6), 0.4, 0., 360.)
    .assemble(tag='face')
    .edges('%LINE', tag='face')
    .vertices()
    .chamfer(0.2)
)
show(result)
# %%
result = (
    cq.Sketch()
    .arc((0,0),1.,0.,360.)
    .arc((1,1.5),0.5,0.,360.)
    .segment((0.,2),(-1,3.))
    .hull()
)
show(result)
# %%
result = (
    cq.Sketch()
    .segment((0,0), (0,3.),"s1")
    .arc((0.,3.), (1.5,1.5), (0.,0.),"a1")
    .constrain("s1","Fixed",None)
    .constrain("s1", "a1","Coincident",None)
    .constrain("a1", "s1","Coincident",None)
    .constrain("s1",'a1', "Angle", 45)
    .solve()
    .assemble()
)
show(result)
# %%
result = (
    cq.Workplane()
    .box(5, 5, 1)
    .faces('>Z')
    .sketch()
    .regularPolygon(2, 3, tag='outer')
    .regularPolygon(1.5, 3, mode='s')
    .vertices(tag='outer')
    .fillet(.2)
    .finalize()
    .extrude(.5)
)
show(result)
# %%
result = (
    cq.Workplane()
    .box(5, 5, 1)
    .faces('>Z')
    .workplane()
    .rarray(2, 2, 2, 2)
    .rect(1.5, 1.5)
    .extrude(.5)
    .faces('>Z')
    .sketch()
    .circle(0.4)
    .wires()
    .distribute(6)
    .circle(0.1, mode='a')
    .clean()
    .finalize()
    .cutBlind(-0.5, taper=10)
)
show(result)

# %%
s = (
    cq.Sketch()
    .trapezoid(3, 1, 110)
    .vertices()
    .fillet(0.2)
)
result = (
    cq.Workplane()
    .box(5, 5, 5)
    .faces('>X')
    .workplane()
    .transformed((0, 0, -90))
    .placeSketch(s)
    .cutThruAll()
)
show(result)
# %%
s1 = (
    cq.Sketch()
    .trapezoid(3, 1, 110)
    .vertices()
    .fillet(0.2)
)

s2 = (
    cq.Sketch()
    .rect(2,1)
    .vertices()
    .fillet(0.2)
)
result = (
    cq.Workplane()
    .placeSketch(s1, s2.moved(cq.Location(cq.Vector(0, 0, 3))))
    .loft()
)
show(result)

# %%