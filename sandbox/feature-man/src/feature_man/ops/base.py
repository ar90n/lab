from abc import ABC, abstractmethod, abstractproperty
from typing import Any, List, Optional, Union, cast, get_origin, overload

import numpy as np

from ..context import Context
from ..exceptions import ArityMismatchError, ShapeValidationError
from ..types import Shape
from .keypoint import create_cv_keypoint, create_keypoint


def get_prefix_msg(io_type: str, position: Optional[int]) -> str:
    if position is not None:
        return f"{position}th {io_type}"
    return io_type


def _validate_shape(
    expect: Shape, actual: Shape, io_type: str, position: Optional[int] = None
) -> None:
    prefix_msg = get_prefix_msg(io_type, position)
    if len(expect) != len(actual):
        raise ShapeValidationError(
            f"number of {prefix_msg} dimensions mismatch: {len(expect)} != {len(actual)}"
        )

    is_valid = all((e == a for e, a in zip(expect, actual) if e is not None))
    if not is_valid:
        raise ShapeValidationError(f"{prefix_msg} shape mismatch: {expect} - {actual}")


def _wrap_by_list_if_need(shapes: Union[List[Shape], Shape]) -> List[Shape]:
    return cast(List[Shape], [shapes] if type(shapes) == get_origin(Shape) else shapes)


class BaseOp(ABC):
    @overload
    def __call__(self, context: Context, shapes: List[Shape]) -> List[Shape]:
        ...

    @overload
    def __call__(self, context: Context, arrays: List[np.ndarray]) -> List[np.ndarray]:
        ...

    def __call__(self, context: Context, *arg: List[Any]):
        if type(arg[0][0]) == get_origin(Shape):
            self._validate_input_shapes(*arg)
            output_shape = self._compute_shape(context, *arg)
            self._validate_output_shape(output_shape)
            return output_shape
        if type(arg[0][0]) == np.ndarray:
            return self._compute_feature(context, *arg)
        raise NotImplementedError(f"{type(arg[0][0])} is not supported.")

    def _compute_shape(self, context:Context, shapes: List[Shape]) -> List[Shape]:
        dummy = [np.zeros(s) for s in shapes]
        return [f.shape for f in self._compute_feature(context, dummy)]

    @abstractmethod
    def _compute_feature(self, context: Context, arrays: List[np.ndarray]) -> List[np.ndarray]:
        ...

    @abstractproperty
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        ...

    @property
    def output_shape(self) -> Optional[Shape]:
        return None

    def _validate_input_shapes(self, *shapes: List[Shape]) -> None:
        expected_shapes = _wrap_by_list_if_need(self.input_shapes)
        if len(shapes) != len(expected_shapes):
            raise ArityMismatchError(
                f"mismatch input arity: {len(shapes)} != {len(expected_shapes)}"
            )

        for i, (e, ss) in enumerate(zip(expected_shapes, shapes), start=1):
            for s in ss:
                _validate_shape(e, s, "input", i)

    def _validate_output_shape(self, shapes: List[Shape]) -> None:
        if self.output_shape is not None:
            for s in shapes:
                _validate_shape(self.output_shape, s, "output")


class BaseDescriptor(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return [(1, None, None), (None, 7)]

    @overload
    def __call__(
        self, image_shapes: List[Shape], keypoints_shapes: List[Shape]
    ) -> List[Shape]:
        ...

    @overload
    def __call__(
        self, context: Context, images: List[np.ndarray], keypoints: List[np.round]
    ) -> List[np.ndarray]:
        ...

    def __call__(self, context: Context, *args):
        return BaseOp.__call__(self, context, *args)

    def _compute_feature(
        self, context: Context, images: List[np.ndarray], keypoints: List[np.ndarray]
    ) -> List[np.ndarray]:
        return [self._compute_description(i, k) for i, k in zip(images, keypoints)]

    @abstractmethod
    def _compute_description(
        self, image: np.ndarray, keypoints: np.ndarray
    ) -> np.ndarray:
        ...


class BaseDetector(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return (1, None, None)

    @property
    def output_shape(self) -> Shape:
        return (self.n_keypoints, 7)

    @property
    def n_keypoints(self) -> Optional[int]:
        return None

    def _compute_feature(self, context: Context, images: List[np.ndarray]) -> List[np.ndarray]:
        return [self._compute_keypoints(i) for i in images]

    @abstractmethod
    def _compute_keypoints(self, image: np.ndarray) -> np.ndarray:
        ...


class OpenCVDescriptor(BaseDescriptor):
    @property
    def output_shape(self) -> Shape:
        return (None, self.descriptor.descriptorSize())

    @property
    def descriptor(self):
        if self._descriptor is None:
            self._descriptor = self._create_descriptor()
        return self._descriptor

    def __init__(self) -> None:
        self._descriptor = None

    def _compute_description(
        self, image: np.ndarray, keypoints: np.ndarray
    ) -> np.ndarray:
        if keypoints.size == 0:
            return np.array([]).reshape(0, self.output_shape[1])

        image = np.squeeze(image)
        cv_keypoints = [create_cv_keypoint(k) for k in keypoints]
        return self.descriptor.compute(image, cv_keypoints)[1]

    def __getstate__(self):
        return {**self.__dict__, "_descriptor": None}

    @abstractmethod
    def _create_descriptor():
        ...


class BaseDetector(BaseOp):
    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return (1, None, None)

    @property
    def output_shape(self) -> Shape:
        return (self.n_keypoints, 7)

    @property
    def n_keypoints(self) -> Optional[int]:
        return None

    def _compute_feature(self, context: Context, images: List[np.ndarray]) -> List[np.ndarray]:
        return [self._compute_keypoints(i) for i in images]

    @abstractmethod
    def _compute_keypoints(self, image: np.ndarray) -> np.ndarray:
        ...


class OpenCVDetector(BaseDetector):
    @property
    def detector(self):
        if self._detector is None:
            self._detector = self._create_detector()
        return self._detector

    def __init__(self) -> None:
        self._detector = None

    def _compute_keypoints(self, image: np.ndarray) -> np.ndarray:
        image = np.squeeze(image)
        keypoints = self.detector.detect(image)
        if len(keypoints) == 0:
            return np.array([]).reshape(0, self.output_shape[1])

        return np.vstack([create_keypoint(k) for k in keypoints])

    def __getstate__(self):
        return {**self.__dict__, "_detector": None}

    @abstractmethod
    def _create_detector():
        ...
