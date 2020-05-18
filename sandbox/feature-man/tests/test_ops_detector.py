import numpy as np
import pytest

from feature_man import Context
from feature_man.ops.detector import GridDetector, RandomDetector


def test_create_GridDetector_instance():
    detector = GridDetector(n_hor_grids=10, n_ver_grids=20, radius=3.0)

    assert 10 == detector.n_hor_grids
    assert 20 == detector.n_ver_grids
    assert 3.0 == detector.radius
    assert (1, None, None) == detector.input_shapes
    assert (200, 7) == detector.output_shape


def test_create_RandomDetector_instance():
    detector = RandomDetector(n_keypoints=32, max_radius=3.3, seed=42)

    assert 32 == detector.n_keypoints
    assert 3.3 == detector.max_radius
    assert 42 == detector.seed
    assert (1, None, None) == detector.input_shapes
    assert (32, 7) == detector.output_shape


@pytest.mark.parametrize(
    "clz, param, given, expect",
    [
        (
            GridDetector,
            {"n_hor_grids": 4, "n_ver_grids": 4, "radius": 3.0},
            [np.zeros((1, 32, 32))],
            [
                np.array(
                    [
                        [4.0, 4.0, 3.0, -1, 0, 0, -1],
                        [4.0, 12.0, 3.0, -1, 0, 0, -1],
                        [4.0, 20.0, 3.0, -1, 0, 0, -1],
                        [4.0, 28.0, 3.0, -1, 0, 0, -1],
                        [12.0, 4.0, 3.0, -1, 0, 0, -1],
                        [12.0, 12.0, 3.0, -1, 0, 0, -1],
                        [12.0, 20.0, 3.0, -1, 0, 0, -1],
                        [12.0, 28.0, 3.0, -1, 0, 0, -1],
                        [20.0, 4.0, 3.0, -1, 0, 0, -1],
                        [20.0, 12.0, 3.0, -1, 0, 0, -1],
                        [20.0, 20.0, 3.0, -1, 0, 0, -1],
                        [20.0, 28.0, 3.0, -1, 0, 0, -1],
                        [28.0, 4.0, 3.0, -1, 0, 0, -1],
                        [28.0, 12.0, 3.0, -1, 0, 0, -1],
                        [28.0, 20.0, 3.0, -1, 0, 0, -1],
                        [28.0, 28.0, 3.0, -1, 0, 0, -1],
                    ]
                )
            ],
        ),
        (
            RandomDetector,
            {"n_keypoints": 4, "max_radius": 3.0, "seed": 42},
            [np.zeros((1, 32, 32))],
            [
                np.array(
                    [
                        [
                            11.61074368,
                            29.4721435,
                            2.19598183,
                            3.76148219,
                            0.0,
                            0.0,
                            -1.0,
                        ],
                        [
                            4.83657785,
                            4.83583013,
                            0.17425084,
                            5.44234523,
                            0.0,
                            0.0,
                            -1.0,
                        ],
                        [
                            18.63456536,
                            21.95024991,
                            0.06175348,
                            6.09412333,
                            0.0,
                            0.0,
                            -1.0,
                        ],
                        [
                            25.80572186,
                            6.58251243,
                            0.5454749,
                            1.15236452,
                            0.0,
                            0.0,
                            -1.0,
                        ],
                    ]
                )
            ],
        ),
    ],
)
def test_detect_keypoints(clz, param, given, expect):
    context = Context(10, 1, 1, 0)
    detector = clz(**param)

    assert detector.output_shape == detector(context, [a.shape for a in given])[0]

    keypoints = detector(context, given)
    for e, a in zip(expect, keypoints):
        assert e.shape == a.shape
        assert np.allclose(e, a)
