# %%
import cadquery as cq
from cq_warehouse import thread

plate = (
    cq.Workplane("XY")
    .sketch()
    .circle(40)
    .circle(12, mode='s')
    .finalize()
    .extrude(15)
)
thread = (
    thread.IsoThread(
        23.99, 3, 15,
        external=False,
        end_finishes=("square","fade"),
    )
)
plate.union(thread)
# %%
