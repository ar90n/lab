# %%
from pathlib import Path

# %%
import numpy as np
import skimage
import matplotlib.pyplot as plt

# %%
stricker_image_path = Path.cwd() / "IMG_8839.jpg" # dia
#stricker_image_path = Path.cwd() / "IMG_8846.jpg" # club
#stricker_image_path = Path.cwd() / "IMG_8849.jpg" # flower
#stricker_image_path = Path.cwd() / "IMG_8850.jpg" # spade
#stricker_image_path = Path.cwd() / "IMG_8851.jpg" # heart
# %%
stricker_image = skimage.io.imread(stricker_image_path)
# %%
mm_per_pix = 0.1
paddind_pix = 100
#bounding_box_shape_mm = (60, 70)
bounding_box_shape_mm = (50, 60)
bounding_box_shape_pix = np.array(
    [
        int(bounding_box_shape_mm[0] / mm_per_pix),
        int(bounding_box_shape_mm[1] / mm_per_pix),
    ]
)
bounding_box = np.array(
    [
        [0, 0],
        [0, bounding_box_shape_pix[1]],
        [0 + bounding_box_shape_pix[0], bounding_box_shape_pix[1]],
        [0 + bounding_box_shape_pix[0], 0],
    ]
) + paddind_pix
# %%
anker_points = np.array(
    [
        # IMG_8835.jpg
        #[652, 122],
        #[512, 2796],
        #[2916, 2836],
        #[2884, 118],
        # IMG_8839.jpg
        [392, 1004],
        [360, 3788],
        [2732, 3748],
        [2648, 1000],
        # IMG_8846.jpg
        #[432, 762],
        #[483, 3423],
        #[2769, 3324],
        #[2604, 705],
        ## IMG_8849.jpg
        #[432, 504],
        #[282, 3318],
        #[2700, 3288],
        #[2622, 528],
        ## IMG_8849.jpg
        #[303, 534],
        #[387, 3354],
        #[2790, 3267],
        #[2613, 480],
        ## IMG_8849.jpg
        #[300, 669],
        #[255, 3660],
        #[2775, 3585],
        #[2691, 684],
    ]
)
# %%
src = bounding_box
dst = anker_points

tform3 = skimage.transform.ProjectiveTransform()
tform3.estimate(src, dst)
warped = skimage.transform.warp(stricker_image, tform3, output_shape=(bounding_box_shape_pix[1] + 2 * paddind_pix, bounding_box_shape_pix[0] + 2 * paddind_pix))

# %%
plt.imshow(warped)
# %%
from skimage.segmentation import flood, flood_fill
from skimage.measure import label, regionprops

hsv = skimage.color.rgb2hsv(warped)
center = (np.array(hsv.shape[:2]) / 2).astype(int)
basis = hsv[center[0], center[1], 0]
#filled_hsv = flood_fill(hsv[:,:,1], tuple(center), basis, tolerance=0.068) 
filled_hsv = flood_fill(hsv[:,:,0], tuple(center), basis, tolerance=0.30)
region = max(regionprops(label(filled_hsv == basis)), key=lambda x: x.area)
mask = np.zeros(filled_hsv.shape)
mask[region.bbox[0]:region.bbox[2], region.bbox[1]:region.bbox[3]] = region.image_filled
mask = np.fliplr(mask)
# %%
plt.imshow(mask)
# %%
margin_mm = 3.5
wall_mm = 1.0
base_wall_mm = 3.0
margin_pix = int(margin_mm / mm_per_pix)
wall_pix = int(wall_mm / mm_per_pix)
base_wall_pix = int(base_wall_mm / mm_per_pix)
# %%
from scipy.ndimage import gaussian_filter
import skimage.morphology
inner = 0.1 < gaussian_filter(mask.astype(np.float32), sigma=22, radius=margin_pix)
outer = skimage.morphology.binary_dilation(inner, skimage.morphology.disk(wall_pix))
base = skimage.morphology.binary_dilation(inner, skimage.morphology.disk(base_wall_pix))

# %%
import skimage.morphology
inner_edge = inner ^ skimage.morphology.erosion(inner, skimage.morphology.disk(1))
outer_edge = outer ^ skimage.morphology.erosion(outer, skimage.morphology.disk(1))
base_edge = base ^ skimage.morphology.erosion(base, skimage.morphology.disk(1))
# %%
plt.imshow(inner_edge)
# %%
plt.imshow(outer_edge)
# %%
plt.imshow(base_edge)
# %%
inner_points = np.vstack(np.where(inner_edge)[::-1]).T
outer_points = np.vstack(np.where(outer_edge)[::-1]).T
base_points = np.vstack(np.where(base_edge)[::-1]).T
# %%
inner_seed = inner_points[0]
work_img = np.copy(inner_edge)

ret = [inner_seed]
work_img[ret[-1][0], ret[-1][1]] = False
while 0 < np.sum(work_img):
    has_change = False
    for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (-1, 1), (1, -1)]:
        new_point = ret[-1] + np.array([dx, dy])
        if work_img[new_point[1], new_point[0]]:
            ret.append(new_point)
            work_img[new_point[1], new_point[0]] = False
            has_change = True
    if not has_change:
        break
inner_points = np.array(ret)
# %%
outer_seed = outer_points[0]
work_img = np.copy(outer_edge)

ret = [outer_seed]
work_img[ret[-1][0], ret[-1][1]] = False
while 0 < np.sum(work_img):
    for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (-1, 1), (1, -1)]:
        new_point = ret[-1] + np.array([dx, dy])
        if work_img[new_point[1], new_point[0]]:
            ret.append(new_point)
            work_img[new_point[1], new_point[0]] = False
outer_points = np.array(ret)

# %%
base_seed = base_points[0]
work_img = np.copy(base_edge)

ret = [base_seed]
work_img[ret[-1][0], ret[-1][1]] = False
while 0 < np.sum(work_img):
    for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (-1, 1), (1, -1)]:
        new_point = ret[-1] + np.array([dx, dy])
        if work_img[new_point[1], new_point[0]]:
            ret.append(new_point)
            work_img[new_point[1], new_point[0]] = False
base_points = np.array(ret)


# %%
inner_points_mm = mm_per_pix * inner_points
outer_points_mm = mm_per_pix * outer_points
base_points_mm = mm_per_pix * base_points
inner_pts = [(float(x), float(y)) for x, y in inner_points_mm]
outer_pts = [(float(x), float(y)) for x, y in outer_points_mm]
base_pts = [(float(x), float(y)) for x, y in base_points_mm]

# %%
from build123d import *

# %%
with BuildPart() as part:
    with BuildSketch(Plane.XY) as base_sk:
        with BuildLine() as base_ln:
            l1 = Polyline(*base_pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
    extrude(amount=1)
    with BuildSketch(Plane.XY) as outer_sk:
        with BuildLine() as outer_ln:
            l1 = Polyline(*outer_pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
        with Locations(outer_ln.edges().last.vertices()[0].to_tuple()) as l:
            Circle(5)
    extrude(amount=5)
    with BuildSketch(Plane.XY) as inner_sk:
        with BuildLine() as inner_ln:
            l1 = Polyline(*inner_pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
        with Locations(outer_ln.edges().last.vertices()[0].to_tuple()):
            Circle(4)
        with Locations(outer_sk.vertices().sort_by(Axis.Y).first.to_tuple()):
            Circle(0.5)
    extrude(amount=5, mode=Mode.SUBTRACT)

# %%
part.part
# %%
part.part.export_stl("dia.stl")
# %%
