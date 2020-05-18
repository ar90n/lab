from enum import Enum, IntEnum
from typing import Dict, Tuple

import cv2
import numpy as np

from .base import OpenCVDescriptor, OpenCVDetector
from .keypoint import KeyPointColumn


class AKAZEDetector(OpenCVDetector):
    class Diffusivity(IntEnum):
        PM_G1 = cv2.KAZE_DIFF_PM_G1
        PM_G2 = cv2.KAZE_DIFF_PM_G2
        WEICKERT = cv2.KAZE_DIFF_WEICKERT
        CHARBONNIER = cv2.KAZE_DIFF_CHARBONNIER

    def __init__(
        self,
        threshold: float = 0.001,
        n_octaves: int = 4,
        n_octave_layers: int = 4,
        diffusivity: Diffusivity = Diffusivity.PM_G2,
    ) -> None:
        super().__init__()
        self.threshold = threshold
        self.n_octaves = n_octaves
        self.n_octave_layers = n_octave_layers
        self.diffusivity = diffusivity

    def _create_detector(self):
        return cv2.AKAZE_create(
            threshold=self.threshold,
            nOctaves=self.n_octaves,
            nOctaveLayers=self.n_octave_layers,
            diffusivity=self.diffusivity,
        )


class AKAZEDescriptor(OpenCVDescriptor):
    class DescriptorType(Enum):
        KAZE = 0
        MLDB = 1

    class Diffusivity(IntEnum):
        PM_G1 = cv2.KAZE_DIFF_PM_G1
        PM_G2 = cv2.KAZE_DIFF_PM_G2
        WEICKERT = cv2.KAZE_DIFF_WEICKERT
        CHARBONNIER = cv2.KAZE_DIFF_CHARBONNIER

    @classmethod
    def _get_cv_descriptor_type(
        cls, descriptor_type: DescriptorType, upright: bool
    ) -> int:
        table: Dict[Tuple[cls.DescriptorType, bool], int] = {
            (cls.DescriptorType.KAZE, True): cv2.AKAZE_DESCRIPTOR_KAZE_UPRIGHT,
            (cls.DescriptorType.KAZE, False): cv2.AKAZE_DESCRIPTOR_KAZE,
            (cls.DescriptorType.MLDB, True): cv2.AKAZE_DESCRIPTOR_MLDB_UPRIGHT,
            (cls.DescriptorType.MLDB, False): cv2.AKAZE_DESCRIPTOR_MLDB,
        }
        return table[(descriptor_type, upright)]

    def __init__(
        self,
        descriptor_type: DescriptorType = DescriptorType.MLDB,
        descriptor_size: int = 0,
        n_octaves: int = 4,
        n_octave_layers: int = 4,
        upright: bool = False,
        diffusivity: Diffusivity = Diffusivity.PM_G2,
        default_class_id: int = 1,
    ):
        super().__init__()
        self.descriptor_type = descriptor_type
        self.descriptor_size = descriptor_size
        self.n_octaves = n_octaves
        self.n_octave_layers = n_octave_layers
        self.descriptor_type = self._get_cv_descriptor_type(descriptor_type, upright)
        self.diffusivity = diffusivity
        self._default_class_id = default_class_id

    def _create_descriptor(self):
        return cv2.AKAZE_create(
            descriptor_type=self.descriptor_type,
            descriptor_size=self.descriptor_size,
            nOctaves=self.n_octaves,
            nOctaveLayers=self.n_octave_layers,
            diffusivity=self.diffusivity,
        )

    def _compute_description(
        self, image: np.ndarray, keypoints: np.ndarray
    ) -> np.ndarray:
        image = np.squeeze(image)
        cv_keypoints = [self._create_cv_keypoint(k) for k in keypoints]
        return self.descriptor.compute(image, cv_keypoints)[1]

    def _create_cv_keypoint(self, keypoint: np.ndarray) -> cv2.KeyPoint:
        class_id = (
            self._default_class_id
            if keypoint[KeyPointColumn.CLASS_ID] == -1
            else keypoint[KeyPointColumn.CLASS_ID]
        )
        return cv2.KeyPoint(
            keypoint[KeyPointColumn.X],
            keypoint[KeyPointColumn.Y],
            keypoint[KeyPointColumn.SIZE],
            keypoint[KeyPointColumn.ANGLE],
            int(keypoint[KeyPointColumn.RESPONSE]),
            int(keypoint[KeyPointColumn.OCTAVE]),
            int(class_id),
        )
