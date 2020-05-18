import pytest
from typing import Dict, Any
from typing import List, overload

from feature_man import Context
from feature_man.ops import BaseOp
from feature_man.types import Shape
from feature_man.exceptions import ShapeValidationError, ArityMismatchError

import numpy as np


def _create_op_class(namespace: Dict[str, Any]):
    return type(
        "_Clz", (BaseOp,), {"_compute_feature": lambda self, context, x: x, **namespace,},
    )


@pytest.mark.parametrize(
    "namespace, given, expect",
    [
        (
            {
                "_compute_shape": lambda self, context, xs: [
                    (x[0], 2 * x[1], 2 * x[2]) for x in xs
                ],
                "input_shapes": property(lambda self: (1, None, None)),
            },
            [[(1, 2, 3), (1, 100, 200)]],
            [(1, 4, 6), (1, 200, 400)],
        ),
        (
            {
                "_compute_shape": lambda self, context, xs: [(x[0], 128) for x in xs],
                "input_shapes": property(lambda self: [(1, None, None)]),
                "output_shape": property(lambda self: (1, 128)),
            },
            [[(1, 2, 3)]],
            [(1, 128)],
        ),
        (
            {
                "_compute_shape": lambda self, context, xs, _: [(x[0], 128) for x in xs],
                "input_shapes": property(lambda self: [(1, None, None), (None, 3)]),
                "output_shape": property(lambda self: (1, 128)),
            },
            [[(1, 2, 3)], [(123, 3)]],
            [(1, 128)],
        ),
    ],
)
def test_shape_transform(namespace, given, expect):
    c = _create_op_class(namespace)()
    context = Context(10, 1, 1, 0)
    assert expect == c(context, *given)


@pytest.mark.parametrize(
    "namespace, given, expect",
    [
        (
            {"input_shapes": property(lambda self: [(1, None, None)]),},
            [[(1, 2)], [(1, 2)]],
            (ArityMismatchError, "mismatch input arity: 2 != 1"),
        ),
        (
            {"input_shapes": property(lambda self: [(1, None, None), (1, None, None)]),},
            [[(1, 128, 128)], [(128, 2)]],
            (
                ShapeValidationError,
                "number of 2th input dimensions mismatch: 3 != 2",
            ),
        ),
        (
            {"input_shapes": property(lambda self: (1, None, None)),},
            [[(3, 3, 2)]],
            (ShapeValidationError, "1th input shape mismatch: (1, None, None) - (3, 3, 2)"),
        ),
        (
            {
                "input_shapes": property(lambda self: (1, None, None)),
                "output_shape": property(lambda self: (3, 128)),
            },
            [[(1, 2, 3)]],
            (
                ShapeValidationError,
                "number of output dimensions mismatch: 2 != 3",
            ),
        ),
        (
            {
                "input_shapes": property(lambda self: (1, None, None)),
                "output_shape": property(lambda self: (3, None, None)),
            },
            [[(1, 2, 3)]],
            (
                ShapeValidationError,
                "output shape mismatch: (3, None, None) - (1, 2, 3)",
            ),
        ),
    ],
)
def test_shape_transform_validataion(namespace, given, expect):
    context = Context(10, 1, 1, 0)
    with pytest.raises(Exception) as e:
        _create_op_class(namespace)()(context, *given)

    expect_type, expect_msg = expect
    assert e.type is expect_type
    assert e.value.args[0] == expect_msg
