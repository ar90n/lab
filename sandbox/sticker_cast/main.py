# %%
from pathlib import Path

# %%
import numpy as np
import skimage
import matplotlib.pyplot as plt

# %%
stricker_image_path = Path.cwd() / "IMG_8835.jpg"
# %%
stricker_image = skimage.io.imread(stricker_image_path)
# %%
mm_per_pix = 0.1
bounding_box_shape_mm = (60, 70)
bounding_box_shape_pix = np.array(
    [
        int(bounding_box_shape_mm[0] / mm_per_pix),
        int(bounding_box_shape_mm[1] / mm_per_pix),
    ]
)
bounding_box = np.array(
    [
        [25, 0],
        [25, bounding_box_shape_pix[1]],
        [25 + bounding_box_shape_pix[0], bounding_box_shape_pix[1]],
        [25 + bounding_box_shape_pix[0], 0],
    ]
)
# %%
anker_points = np.array(
    [
        [652, 122],
        [512, 2796],
        [2916, 2836],
        [2884, 118],
    ]
)
# %%
src = bounding_box
dst = anker_points

tform3 = skimage.transform.ProjectiveTransform()
tform3.estimate(src, dst)
warped = skimage.transform.warp(stricker_image, tform3, output_shape=(bounding_box_shape_pix[1], bounding_box_shape_pix[0]))

# %%
from skimage.segmentation import flood, flood_fill
hsv = skimage.color.rgb2hsv(warped)
center = (np.array(hsv.shape[:2]) / 2).astype(int)
basis = hsv[center[0], center[1], 1]
filled_hsv = flood_fill(hsv[:,:,1], tuple(center), basis, tolerance=0.08)
mask = filled_hsv == basis

# %%
margin_mm = 2
wall_mm = 1
margin_pix = int(margin_mm / mm_per_pix)
wall_pix = int(wall_mm / mm_per_pix)
# %%
from scipy.ndimage import gaussian_filter
import skimage.morphology
inner = 0.1 < gaussian_filter(mask.astype(np.float32), sigma=22, radius=margin_pix)
outer = skimage.morphology.binary_dilation(inner, skimage.morphology.disk(wall_pix))

# %%
import skimage.morphology
inner_edge = inner ^ skimage.morphology.erosion(inner, skimage.morphology.disk(1))
outer_edge = outer ^ skimage.morphology.erosion(outer, skimage.morphology.disk(1))
# %%
inner_points = np.vstack(np.where(inner_edge)[::-1]).T
outer_points = np.vstack(np.where(outer_edge)[::-1]).T
# %%
inner_seed = inner_points[0]
work_img = np.copy(inner_edge)

ret = [inner_seed]
work_img[ret[-1][0], ret[-1][1]] = False
while 0 < np.sum(work_img):
    for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (-1, 1), (1, -1)]:
        new_point = ret[-1] + np.array([dx, dy])
        if work_img[new_point[0], new_point[1]]:
            ret.append(new_point)
            work_img[new_point[0], new_point[1]] = False
inner_points = np.array(ret)
# %%
outer_seed = outer_points[0]
work_img = np.copy(outer_edge)

ret = [outer_seed]
work_img[ret[-1][0], ret[-1][1]] = False
while 0 < np.sum(work_img):
    for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (-1, 1), (1, -1)]:
        new_point = ret[-1] + np.array([dx, dy])
        if work_img[new_point[0], new_point[1]]:
            ret.append(new_point)
            work_img[new_point[0], new_point[1]] = False
outer_points = np.array(ret)

# %%
inner_points_mm = mm_per_pix * inner_points
outer_points_mm = mm_per_pix * outer_points
inner_pts = [(float(x), float(y)) for x, y in inner_points_mm]
outer_pts = [(float(x), float(y)) for x, y in outer_points_mm]

# %%
from build123d import *

# %%
with BuildPart() as part:
    with BuildSketch(Plane.XY) as outer_sk:
        with BuildLine() as outer_ln:
            l1 = Polyline(*outer_pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
        #with Locations(outer_ln.edges().last.vertices()[0].to_tuple()):
        #    Circle(3)
    extrude(amount=6)
    with BuildSketch(Plane.XY) as inner_sk:
        with BuildLine() as inner_ln:
            l1 = Polyline(*inner_pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
        #with Locations(outer_ln.edges().last.vertices()[0].to_tuple()):
        #    Circle(2.5)
    extrude(amount=5, mode=Mode.SUBTRACT)

# %%
part.part
# %%
part.part.export_stl("disk.stl")
# %%
