from enum import IntEnum
from typing import Optional, Tuple, cast

import cv2

from .base import OpenCVDetector


class SimpleBlobDetector(OpenCVDetector):
    class BlobColor(IntEnum):
        DARK = 0
        LIGHT = 255

    def __init__(
        self,
        threshold_range: Tuple[float, float] = (50, 220.0),
        threshold_step: float = 10.0,
        blob_color: Optional[BlobColor] = BlobColor.DARK,
        area_range: Optional[Tuple[float, float]] = (25.0, 5000.0),
        circularity_range: Optional[Tuple[float, float]] = None,
        inertia_ratio_range: Optional[Tuple[float, float]] = (
            0.10000000149011612,
            3.4028234663852886e38,
        ),
        convexity_range: Optional[Tuple[float, float]] = (
            0.949999988079071,
            3.4028234663852886e38,
        ),
    ) -> None:
        super().__init__()
        self.threshold_range = threshold_range
        self.threshold_step = threshold_step
        self.blob_color = blob_color
        self.area_range = area_range
        self.circularity_range = circularity_range
        self.inertia_ratio_range = inertia_ratio_range
        self.convexity_range = convexity_range

    def _create_detector(self):
        params = cv2.SimpleBlobDetector_Params()

        params.minThreshold, params.maxThreshold = self.threshold_range
        params.thresholdStep = self.threshold_step
        params.filterByColor = self.blob_color is not None
        if params.filterByColor:
            params.blobColor = self.blob_color

        params.filterByArea = self.area_range is not None
        if params.filterByArea:
            params.minArea, params.maxArea = cast(Tuple[float, float], self.area_range)

        params.filterByCircularity = self.circularity_range is not None
        if params.filterByCircularity:
            params.minCircularity, params.maxCircularity = cast(
                Tuple[float, float], self.circularity_range
            )

        params.filterByInertia = self.inertia_ratio_range is not None
        if params.filterByInertia:
            params.minInertiaRatio, params.maxInertiaRatio = cast(
                Tuple[float, float], self.inertia_ratio_range
            )

        params.filterByConvexity = self.convexity_range is not None
        if params.filterByConvexity:
            params.minConvexity, params.maxConvexity = cast(
                Tuple[float, float], self.convexity_range
            )

        # blob detection only works with "uint8" images.
        return cv2.SimpleBlobDetector_create(params)
