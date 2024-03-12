from importlib.metadata import PackageNotFoundError, version

from . import type, utils, preprocess, simplex

try:
    __version__: str = version(__name__)
except PackageNotFoundError:
    __version__: str = "unknown"

__all__ = [
    "type",
    "utils",
    "preprocess",
    "simplex",
    "__version__",
]
