# %%
import pickle
from pathlib import Path

# %%
import numpy as np
from build123d import *
from ocp_vscode import show

# %%
import skimage.io as io
import skimage.color as color
import skimage.morphology as morph
import skimage.segmentation as seg
import skimage.measure as meas
import skimage.filters as filt
import skimage.draw as draw
import matplotlib.pyplot as plt
# %%
cp_sticker = io.imread("cp_sticker.png")[:, :, :3]
io.imshow(cp_sticker)
# %%
cp_sticker_hsv = color.rgb2hsv(cp_sticker)
sat = cp_sticker_hsv[:, :, 1]
sat_mask = np.abs(sat - sat[100, 350]) < 0.10

hue = cp_sticker_hsv[:, :, 0]
hue_mask = np.abs(hue - hue[100, 350]) < 0.050
# %%
mask = hue_mask & sat_mask
# %%
m2 = mask.copy()
m2 = morph.remove_small_holes(m2, area_threshold=256)
m2 = morph.remove_small_objects(m2, min_size=256, connectivity=0)
m2 = morph.remove_small_holes(m2, area_threshold=256)
io.imshow(m2)
# %%
seg_m = seg.clear_border(m2)
labels = meas.label(seg_m)
sorted_props = sorted(meas.regionprops(labels), key=lambda x: x.area, reverse=True)
circle_reg = sorted_props[0]
name_regs = sorted_props[11:]
#io.imshow(labels)

# %%
circle_mask = labels == circle_reg.label
circle_mask = morph.binary_closing(circle_mask, footprint=morph.disk(3))
circle_mask = morph.binary_opening(circle_mask, footprint=morph.disk(3))
io.imshow(circle_mask)


# %%
reg_masks = []
max_w, max_h = 0, 0
for reg in name_regs:
    reg_mask = labels == reg.label
    reg_mask = morph.binary_closing(reg_mask, footprint=morph.disk(3))
    reg_mask = morph.binary_opening(reg_mask, footprint=morph.disk(3))
    reg_mask = 0.1 < filt.gaussian(reg_mask, sigma=5)

    props = meas.regionprops(meas.label(reg_mask))[0]

    minr, minc, maxr, maxc  = props.bbox
    maxr += 1 if (maxr - minr) % 2 == 1 else 0
    maxc += 1 if (maxc - minc) % 2 == 1 else 0
    reg_mask = reg_mask[minr:maxr, minc:maxc]
    reg_masks.append(reg_mask)

    w = maxc - minc
    h = maxr - minr
    max_w = max(max_w, w)
    max_h = max(max_h, h)

# %%
[a,b,c,d]=circle_reg.bbox
diameter = ((c - a) +  (d - b)) / 2
max_w = max(max_w, diameter)
max_h = max_h + diameter

ratio = 26.0 / diameter
max_w = int(1.5 * max_w)
max_h = int(1.5 * max_h)
# %%
rets = []
margin_pix = 2.1 / ratio
for rm in reg_masks:
    img = np.zeros((max_h, max_w), dtype=np.uint8)
    tm = 100
    tm2 = -5
    cx = max_w // 2
    
    img[tm2 + tm + int(diameter):rm.shape[0] + tm2 + tm + int(diameter), cx - rm.shape[1] // 2:cx + rm.shape[1] // 2] = rm

    rr, cc = draw.disk((tm + diameter // 2, cx), diameter / 2, shape=(max_h, max_w))
    img[rr, cc] = 1

    img = morph.binary_closing(img, footprint=morph.disk(9))
    img = morph.binary_dilation(img, footprint=morph.disk(int(margin_pix + 0.5)))

    rr, cc = draw.disk((tm - 15, cx), 20, shape=(max_h, max_w))
    img[rr, cc] = 1

    rets.append(img)

# %%
reg_contours = []
wall_pix = 2.5 / ratio
for ret in rets:
    inner_reg_contour = ratio * (meas.find_contours(ret, 0.5) - np.array([tm + diameter // 2, cx]))
    outer_mask = morph.binary_dilation(ret, footprint=morph.disk(int(wall_pix)))
    outer_reg_contour = ratio * (meas.find_contours(outer_mask, 0.5) - np.array([tm + diameter // 2, cx]))
    inner_reg_contour = inner_reg_contour.squeeze()
    outer_reg_contour = outer_reg_contour.squeeze()
    reg_contours.append({'inner': inner_reg_contour, 'outer': outer_reg_contour})
# %%
pickle.dump(reg_contours, open("cp_casts.pickle", "wb"))
# %%
