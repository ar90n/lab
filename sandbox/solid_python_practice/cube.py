# %%
from solid import *
from solid.utils import *

c = cube(10)

# %%
scad_render_to_file(c, filepath='cube.scad', include_orig_code=True)

# %%
