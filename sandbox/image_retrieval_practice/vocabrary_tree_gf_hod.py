# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:hydrogen
#     text_representation:
#       extension: .py
#       format_name: hydrogen
#       format_version: '1.3'
#       jupytext_version: 1.4.2
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# %%
%reload_ext autoreload
%autoreload 2

# %%
from smart_open import open
from pathlib import Path
from sklearn.model_selection import train_test_split
from scipy.ndimage import sobel, laplace, convolve, gaussian_filter
from skimage.util.shape import view_as_windows, view_as_blocks
from skimage import io, transform,feature
import numpy as np
import more_itertools
import ray
from sklearn.cluster import KMeans, MiniBatchKMeans
import pickle
import faiss
import matplotlib.pyplot as plt
import datetime

# %%
K = 256 + 32
train_pickle_path = "corner_gf_hog_train_features_20200529143518.pickle"
test_pickle_path = "corner_gf_hog_test_features_20200529143810.pickle"
kmeans_pickle_path = None
train_vlads_pickle_path = None
test_vlads_pickle_path = None

# %%
ray.init()

# %%
HUMANS_SKETCH_OBJECTS_PNG_FILELIST_PATH = "/Volumes/Data/humans_sketch_objects/png/filelist.txt"


# %%
def load_humans_sketch_objects_paths():
    filelist_text = Path(HUMANS_SKETCH_OBJECTS_PNG_FILELIST_PATH).read_text()
    dataset_dir_path = Path(HUMANS_SKETCH_OBJECTS_PNG_FILELIST_PATH).parent
    all_paths = [dataset_dir_path / p for p in filelist_text.split('\n')]
    return train_test_split(all_paths, test_size=0.2)

def create_data_batches(paths, size = 32):
    for chunk in more_itertools.chunked(paths, size):
       yield chunk

def dumps_with_timestamp(obj, name, path = Path.cwd()):
    time_str = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    filename = f"{name}_{time_str}.pickle"
    output_path = Path(path) / filename
    with open(output_path, "wb") as f:
        pickle.dump(obj, f)


# %%
def convolve_flow(cimg: np.ndarray, ckernel: np.ndarray) -> np.ndarray:
    ret_real = convolve(np.real(cimg), np.real(ckernel))
    ret_imag = convolve(np.imag(cimg), np.imag(ckernel))
    return  ret_real + 1j * ret_imag

def calc_flow(edge_map: np.ndarray) -> np.ndarray:
    #edge_map = gaussian_filter(edge_map, sigma=3.0)
    return sobel(edge_map, axis=1, mode="mirror") + 1j * sobel(
        edge_map, axis=0, mode="mirror"
    )

def _calc_gradient_field_kernel() -> np.ndarray:
    return 0.25 * np.array([[0, 1 + 1j, 0], [1 + 1j, 0, 1 + 1j], [0, 1 + 1j, 0]])


def _calc_mask(flow: np.ndarray):
    power = flow * flow.conj()
    return power < 1e-7


def calc_gfc(edge_map: np.ndarray) -> np.ndarray:
    init_flow = calc_flow(edge_map)
    mask = 127 < edge_map
    flow_kernel = _calc_gradient_field_kernel()

    flow = init_flow
    delta_flow = 1j * np.zeros_like(flow)
    for _ in range(np.max(edge_map.shape)):
        next_flow = convolve_flow(flow, flow_kernel)
        if np.max(np.abs(flow[mask] - next_flow[mask])) < 1e-4:
            break
        flow[mask] = next_flow[mask]

    return flow


# %%
# from scikit-learn
def _hog_normalize_block(block, method, eps=1e-5):
    if method == 'L1':
        out = block / (np.sum(np.abs(block), keepdims=True, axis=-1) + eps)
    elif method == 'L1-sqrt':
        out = np.sqrt(block / (np.sum(np.abs(block), keepdims=True, axis=-1) + eps))
    elif method == 'L2':
        out = block / np.sqrt(np.sum(block ** 2, keepdims=True, axis=-1) + eps ** 2)
    elif method == 'L2-Hys':
        out = block / np.sqrt(np.sum(block ** 2, keepdims=True, axis=-1) + eps ** 2)
        out = np.minimum(out, 0.2)
        out = out / np.sqrt(np.sum(out ** 2, keepdims=True, axis=-1) + eps ** 2)
    else:
        raise ValueError('Selected block normalization method is invalid.')

    return out

def _create_patches(img: np.ndarray, center_points: np.ndarray, patch_size) -> np.ndarray:
    left_top_points = np.maximum(0, np.array(center_points) - (patch_size // 2))
    view = view_as_windows(img, (patch_size, patch_size))
    return view[left_top_points[:,0], left_top_points[:,1]]

def _get_bin_centers(n_bins:int) -> np.ndarray:
    bin_center, bin_width = np.linspace(0, 2 * np.pi, num=n_bins,  endpoint=False, retstep=True)
    return bin_center + bin_width / 2 

def calc_keypoints(edge_map: np.ndarray) -> np.ndarray:
    corner_keypoints = feature.corner_peaks(feature.corner_harris(edge_map), min_distance=5, threshold_rel=0)
    line_candidates = np.argwhere(edge_map == 0)
    n_line_keypoints = min(len(line_candidates), 256)
    line_keypoints = line_candidates[np.random.choice(line_candidates.shape[0], n_line_keypoints, replace=False)]
    return np.vstack([corner_keypoints, line_keypoints])


def calc_hog(edge_map: np.ndarray, keypoints: np.ndarray, patch_size: int = 32, n_orientaion:int = 9, cell_size: int = 8, block_size: int = 3) -> np.ndarray:
    grad_flow = calc_gfc(edge_map)
    patches = _create_patches(grad_flow, keypoints, patch_size)
    
    n_cells = patch_size // cell_size
    cells2 = view_as_blocks(np.concatenate(patches, axis=0), (cell_size, cell_size)).reshape(-1, n_cells, n_cells, cell_size, cell_size, 1)
    contrinutions2 = np.maximum(0, np.power(np.cos(_get_bin_centers(n_orientaion) - np.angle(cells2)), 3.0))
    hist_in_cells2 = np.sum(contrinutions2, axis=(-3, -2))
    
    n_blocks = (patch_size // cell_size - block_size + 1) * (patch_size // cell_size - block_size + 1)
    block_feature_size =  n_orientaion * block_size * block_size
    hist_in_blocks2 = view_as_windows(hist_in_cells2, (1, block_size, block_size, n_orientaion))
    hist_in_blocks2 = hist_in_blocks2.reshape(-1, n_blocks, block_feature_size)
    nhist_in_blocks2 = _hog_normalize_block(hist_in_blocks2, "L2-Hys")
    
    m = hist_in_cells2
    half_cell_size = cell_size // 2
    n_patches = patches.shape[0]
    p = ((half_cell_size - 1) * (np.cos(_get_bin_centers(n_orientaion)) + 1j * np.sin(_get_bin_centers(n_orientaion)))) + (half_cell_size + half_cell_size * 1j)
    buf = np.zeros((n_patches, n_cells, n_cells, cell_size, cell_size))
    buf[:, :, :, np.imag(p).astype(np.int), np.real(p).astype(np.int)] = m
    buf.shape, np.concatenate(buf, axis=2).shape
    r = np.concatenate(np.concatenate(buf.transpose([1, 3, 2, 4, 0]), axis=0).transpose([1, 2, 0,3]), axis=0).transpose([2,1,0])
    vis = 10 * np.angle(patches) + r
    
    feature_size = n_blocks * block_feature_size
    return nhist_in_blocks2.reshape(-1, feature_size)#, vis


# %%
# from https://github.com/mxbi/PyVLAD/blob/master/VLADlib/VLAD.py
def _vlad(X,visualDictionary):

    # cluster_centers_ must be writable
    visualDictionary.cluster_centers_ =  visualDictionary.cluster_centers_.copy()
    visualDictionary.cluster_centers_.flags.writeable = True
        
    centers = visualDictionary.cluster_centers_
    labels=visualDictionary.labels_
    k=visualDictionary.n_clusters
    
    predictedLabels = visualDictionary.predict(X)
    m,d = X.shape
    V=np.zeros([k,d])
    #computing the differences

    # for all the clusters (visual words)
    for i in range(k):
        # if there is at least one descriptor in that cluster
        if np.sum(predictedLabels==i)>0:
            # add the diferences
            V[i]=np.sum(X[predictedLabels==i,:]-centers[i],axis=0)


    V = V.flatten()
    # power normalization, also called square-rooting normalization
    V = np.sign(V)*np.sqrt(np.abs(V))

    # L2 normalization

    V = V/np.sqrt(np.dot(V,V))
    return V

@ray.remote
def _calc_vlad(key_features, kmeans):
    result = []
    for k, v in key_features:
        if v is None:
            result.append((k, v))
            continue
        result.append((k, _vlad(v, kmeans)))
    return result

def calc_vlads(features, kmeans):
    result = []
    for chunk in more_itertools.chunked(features.items(), 64):
        result.append(_calc_vlad.remote(chunk, kmeans))
    result = ray.get(result)
    return dict(sum(result, []))


# %%
def _resize_img(img: np.ndarray, size:int = 400, as_binary: bool = True) -> np.ndarray:
        ratio = size / np.max(img.shape)
        resized_shape = (int(ratio * img.shape[0]), int(ratio * img.shape[1]))
        resized_img = transform.resize(img, resized_shape)
        if as_binary:
            resized_img = (255 * (0.95 < resized_img)).astype(np.int)
        return resized_img

@ray.remote
def _calc_feature(batch):
    ret = {}
    for p in batch:
        edge_map = _resize_img(io.imread(p, as_gray=True))
        keypoints = calc_keypoints(edge_map)
        ret[p] = calc_hog(edge_map, keypoints, 32)
    return ret

def calc_features(paths):
    results = {}
    rem_ids = [_calc_feature.remote(batch) for batch in create_data_batches(paths)]
    while 0 < len(rem_ids):
        fin_ids, rem_ids = ray.wait(rem_ids, num_returns=1)
        results.update(*ray.get(fin_ids))
    return results

def train_kmeans(train_features, K):
    all_train_features = np.vstack([f for f in train_features.values() if f is not None])
    return MiniBatchKMeans(n_clusters=K, tol=0.0001, verbose=1).fit(all_train_features)


# %%
train, test = load_humans_sketch_objects_paths()

# %%
hog = calc_features(train[:2])

# %%
list(hog.values())[0].shape

# %%
train=train[:2048]
test=test[:128]

# %%
if train_pickle_path is None:
    train_features = calc_features(train)
    dumps_with_timestamp(train_features, "corner_gf_hog_train_features")
else:
    with open(train_pickle_path, "rb") as f:
        train_features = pickle.load(f)

# %%
if test_pickle_path is None:
    test_features = calc_features(test)
    dumps_with_timestamp(test_features, "corner_gf_hog_test_features")
else:
    with open(test_pickle_path, "rb") as f:
        test_features = pickle.load(f)

# %%
if kmeans_pickle_path is None:
    kmeans = train_kmeans(train_features, K)
    dumps_with_timestamp(kmeans, "corner_gf_hog_kmeans")
else:
    with open(kmeans_pickle_path, "rb") as f:
        kmeans = pickle.load(f)

# %%
if train_vlads_pickle_path is None:
    train_vlads = calc_vlads(train_features, kmeans)
    dumps_with_timestamp(train_vlads, "corner_gf_hog_train_vlads")
else:
    with open(train_vlads_pickle_path, "rb") as f:
        train_vlads = pickle.load(f)

# %%
if test_vlads_pickle_path is None:
    test_vlads = calc_vlads(test_features, kmeans)
    dumps_with_timestamp(test_vlads, "corner_gf_hog_test_vlads")
else:
    with open(test_vlads_pickle_path, "rb") as f:
        test_vlads = pickle.load(f)


# %%
def train_db(features):
    k, v = zip(*[(k, v) for k, v in features.items() if v is not None])
    v = np.vstack(v)
    nn = faiss.IndexFlatL2(v.shape[1])
    nn.add(v.astype(np.float32))
    return nn, k

def query_db(nn, paths, q, k = 9):
    _, I = nn.search(q[np.newaxis, :], k)
    return [paths[i] for i in np.squeeze(I)]


# %%
nn, paths = train_db(train_vlads)

# %%
idx = 95
qk, qv = list(test_vlads.items())[idx]
qv = qv.astype(np.float32)

# %%
result_paths = query_db(nn, paths, qv)

# %%
io.imshow(io.imread(qk))

# %%
plt.subplots(3,3,figsize=(12,12))
for i,p in enumerate(result_paths):
    plt.subplot(3,3,i + 1)
    io.imshow(io.imread(p))
plt.show()

# %%
list(train_features.values())[0].shape

# %%
wi = _resize_img(io.imread(train[0], as_gray=True))

# %%
np.argwhere(wi == 0)

# %%
candidates = np.argwhere(wi == 0)
n_keypoints = min(len(candidates), 800)
key_points = candidates[np.random.choice(candidates.shape[0], n_keypoints, replace=False)]

# %%
candidates

# %%
e = candidates[:,0] + 1j * candidates[:,1]
e = e.reshape(-1,1)
e = e - e. T

# %%
np.min(np.sort(e, axis=1))

# %%
r = np.average(np.sort(np.abs(e) ,axis=1)[:,:5], axis=1)

# %%

# %%
