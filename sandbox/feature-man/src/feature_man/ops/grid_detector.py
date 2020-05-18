import itertools
from typing import Optional

import numpy as np

from . import BaseDetector


class GridDetector(BaseDetector):
    @property
    def n_keypoints(self) -> Optional[int]:
        return self.n_hor_grids * self.n_ver_grids

    def __init__(
        self, n_hor_grids: int, n_ver_grids: int, radius: Optional[float] = None
    ) -> None:
        self.n_hor_grids = n_hor_grids
        self.n_ver_grids = n_ver_grids
        self.radius = radius

    def _compute_keypoints(self, array: np.ndarray) -> np.ndarray:
        _, h, w = array.shape

        hor_step = w / self.n_hor_grids
        ver_step = h / self.n_ver_grids
        r = min(hor_step, ver_step) if self.radius is None else self.radius

        xs = ((i + 0.5) * hor_step for i in range(self.n_hor_grids))
        ys = ((i + 0.5) * ver_step for i in range(self.n_ver_grids))

        # x, y, size, angle, response, octave, class_id
        return np.array([[x, y, r, -1, 0, 0, -1] for x, y in itertools.product(xs, ys)])
