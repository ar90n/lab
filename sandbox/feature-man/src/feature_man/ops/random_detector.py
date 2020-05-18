import math
from typing import Optional

import numpy as np

from . import BaseDetector


class RandomDetector(BaseDetector):
    @property
    def n_keypoints(self) -> Optional[int]:
        return self._n_keypoints

    def __init__(self, n_keypoints: int, max_radius: float, seed: Optional[int] = None):
        self._n_keypoints = n_keypoints
        self.max_radius = max_radius
        self.seed = seed

    def _compute_keypoints(self, feature: np.ndarray) -> np.ndarray:
        _, h, w = feature.shape

        if self.seed is not None:
            np.random.seed(self.seed)
        x_y_size_angle = np.random.rand(self.n_keypoints, 4) @ np.diag(
            [w - 1, h - 1, self.max_radius, 2 * math.pi]
        )
        res_oct_cls = np.repeat([[0, 0, -1]], self.n_keypoints, axis=0)

        # x, y, size, angle, response, octave, class_id
        return np.hstack([x_y_size_angle, res_oct_cls])
