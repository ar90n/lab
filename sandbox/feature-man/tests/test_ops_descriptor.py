import numpy as np
import pytest

from feature_man.ops.descriptor import KAZEDescriptor


def test_create_KAZEDescriptor_instance():
    descriptor = KAZEDescriptor(
        n_octaves=4,
        n_octave_layers=4,
        extended=False,
        upright=False,
        diffusivity=KAZEDescriptor.Diffusivity.PM_G2
    )

    assert [(1, None, None), (None, 7)] == descriptor.input_shapes
    assert (None, 64) == descriptor.output_shape
