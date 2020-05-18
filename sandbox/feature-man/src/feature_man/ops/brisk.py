import cv2

from .base import OpenCVDescriptor, OpenCVDetector


class BRISKDetector(OpenCVDetector):
    def __init__(self, threshold: int = 30, n_octaves: int = 3) -> None:
        super().__init__()
        self.threshold = threshold
        self.n_octaves = n_octaves

    def _create_detector(self):
        return cv2.BRISK_create(thresh=self.threshold, octaves=self.n_octaves)


class BRISKDescriptor(OpenCVDescriptor):
    def __init__(self, pattern_scale: float = 1.0):
        super().__init__()
        self.pattern_scale = pattern_scale

    def _create_descriptor(self):
        return cv2.BRISK_create(patternScale=self.pattern_scale)
