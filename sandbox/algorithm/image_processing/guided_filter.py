import numpy as np
from skimage import data
from skimage.io import imsave

def mean_2d(img, n=8):
    ret = np.cumsum(img, axis=1)
    ret[:, n:] -= ret[:, :-n]

    ret = np.cumsum(ret, axis=0)
    ret[n:, :] -= ret[:-n, :]

    return (ret / (n * n))

img = data.cat()
r, g, b = img.transpose([2,0,1])

i = r / 255.0
p = r / 255.0
mean_i = mean_2d(i)
mean_p = mean_2d(p)
corr_i = mean_2d(i * i)
corr_ip = mean_2d(i * p)

var_i = corr_i - mean_i * mean_i
cov_ip = corr_ip - mean_i * mean_p

eps = 0.1 ** 2 
a = cov_ip / (var_i + eps)
b = mean_p - a * mean_i

mean_a = mean_2d(a)
mean_b = mean_2d(b)

q = mean_a * i + mean_b
imsave("output.png", q)