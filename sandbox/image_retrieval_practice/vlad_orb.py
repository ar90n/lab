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
from skimage import io
from skimage.color import rgb2gray
import numpy as np
import more_itertools
import ray
import cv2
from sklearn.datasets import make_classification
from sklearn.cluster import KMeans
import pickle
import faiss
import matplotlib.pyplot as plt
import datetime

# %%
K = 256
kmeans_pickle_path = None
train_vlads_pickle_path = None
test_vlads_pickle_path = None

# %%
ray.init(num_cpus=8)

# %%
PARIS_DATASET_PATH = "/Volumes/Data/paris"


# %%
def load_paris_paths():
    all_paths = list(Path(PARIS_DATASET_PATH).glob("*/*"))
    return train_test_split(all_paths, test_size=0.2)

def create_data_batches(paths, size = 32):
    for chunk in more_itertools.chunked(paths, size):
        d = {}
        for p in chunk:
            d[str(p)] = cv2.imread(str(p), 0)
        yield d

def dumps_with_timestamp(obj, name, path = Path.cwd()):
    time_str = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    filename = f"{name}_{time_str}.pickle"
    output_path = Path(path) / filename
    with open(output_path, "wb") as f:
        pickle.dump(obj, f)


# %%
@ray.remote
def _calc_feature(batch):
    engine = cv2.ORB_create()

    ret = {}
    for k, img in batch.items():
        ret[k] = engine.detectAndCompute(img, None)[1]
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
    return KMeans(n_clusters=K, init='k-means++', tol=0.0001, verbose=1).fit(all_train_features)


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


# %%
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
train, test = load_paris_paths()

# %%
train_features = calc_features(train)
dumps_with_timestamp(train_features, "train_features")

# %%
test_features = calc_features(test)
dumps_with_timestamp(test_features, "test_features")

# %%
if kmeans_pickle_path is None:
    kmeans = train_kmeans(train_features, K)
    dumps_with_timestamp(kmeans, "vlad_orb_kmeans")
else:
    with open(kmeans_pickle_path, "rb") as f:
        kmeans = pickle.load(f)

# %%
if train_vlads_pickle_path is None:
    train_vlads = calc_vlads(train_features, kmeans)
    dumps_with_timestamp(train_vlads, "train_vlads")
else:
    with open(train_vlads_pickle_path, "rb") as f:
        train_vlads = pickle.load(f)

# %%
if test_vlads_pickle_path is None:
    test_vlads = calc_vlads(test_features, kmeans)
    dumps_with_timestamp(test_vlads, "test_vlads")
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
idx = 32
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
