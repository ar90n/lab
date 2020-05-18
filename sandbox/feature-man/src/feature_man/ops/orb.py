from enum import IntEnum
from typing import Literal

import cv2

from .base import OpenCVDescriptor, OpenCVDetector


class ORBDetector(OpenCVDetector):
    class ScoreType(IntEnum):
        HARRIS = cv2.ORB_HARRIS_SCORE
        FAST = cv2.ORB_FAST_SCORE

    def __init__(
        self,
        n_max_keypoints: int = 500,
        scale_factor: float = 1.2,
        n_levels: int = 8,
        edge_threshold: int = 31,
        first_level: int = 0,
        score_type: ScoreType = ScoreType.HARRIS,
        fast_threshold: int = 20,
    ) -> None:
        super().__init__()
        self.n_max_keypoints = n_max_keypoints
        self.scale_factor = scale_factor
        self.n_levels = n_levels
        self.edge_threshold = edge_threshold
        self.first_level = first_level
        self.score_type = score_type
        self.fast_threshold = fast_threshold

    def _create_detector(self):
        return cv2.ORB_create(
            nfeatures=self.n_max_keypoints,
            scaleFactor=self.scale_factor,
            nlevels=self.n_levels,
            edgeThreshold=self.edge_threshold,
            firstLevel=self.first_level,
            scoreType=self.score_type,
            fastThreshold=self.fast_threshold,
        )


class ORBDescriptor(OpenCVDescriptor):
    def __init__(self, wta_k: Literal[2, 3, 4] = 2, patch_size: int = 31):
        super().__init__()
        self.wta_k = wta_k
        self.patch_size = patch_size

    def _create_descriptor(self):
        return cv2.ORB_create(WTA_K=self.wta_k, patchSize=self.patch_size)
