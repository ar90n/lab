from typing import List, Optional, Union

import numpy as np
from sklearn.cluster import KMeans as _KMeans

from ..context import Context
from ..types import Shape
from .base import BaseOp


class KMeansClustering(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return (None, None)

    @property
    def output_shape(self) -> Optional[Shape]:
        return (self.n_clusters, None)

    def __init__(self, n_clusters: int, random_state: int = 42) -> None:
        super().__init__()
        self.n_clusters = n_clusters
        self.random_state = random_state

    def _compute_feature(
        self, context: Context, arrays: List[np.ndarray]
    ) -> List[np.ndarray]:
        return [self._compute_centroids(a) for a in arrays]

    def _compute_centroids(self, array: np.ndarray) -> np.ndarray:
        array = np.squeeze(array)
        kmeans = _KMeans(n_clusters=self.n_clusters, random_state=self.random_state)
        kmeans.fit(array)
        return kmeans.cluster_centers_
