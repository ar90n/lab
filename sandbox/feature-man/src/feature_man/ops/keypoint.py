from enum import IntEnum

import cv2
import numpy as np


class KeyPointColumn(IntEnum):
    X = 0
    Y = 1
    SIZE = 2
    ANGLE = 3
    RESPONSE = 4
    OCTAVE = 5
    CLASS_ID = 6


def create_cv_keypoint(keypoint: np.ndarray) -> cv2.KeyPoint:
    return cv2.KeyPoint(
        keypoint[KeyPointColumn.X],
        keypoint[KeyPointColumn.Y],
        keypoint[KeyPointColumn.SIZE],
        keypoint[KeyPointColumn.ANGLE],
        int(keypoint[KeyPointColumn.RESPONSE]),
        int(keypoint[KeyPointColumn.OCTAVE]),
        int(keypoint[KeyPointColumn.CLASS_ID]),
    )


def create_keypoint(keypoint: cv2.KeyPoint) -> np.ndarray:
    return np.array(
        [
            keypoint.pt[0],
            keypoint.pt[1],
            keypoint.size,
            keypoint.angle,
            keypoint.response,
            keypoint.octave,
            keypoint.class_id,
        ]
    )
