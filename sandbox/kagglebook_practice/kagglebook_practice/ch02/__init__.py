from . import ch02_01_metrics, ch02_02_custom_usage, ch02_03_optimize, ch02_04_optimize_cv
from collections import OrderedDict

SECTIONS = OrderedDict([
    ("01-metrics", ch02_01_metrics.app),
    ("02-custom-usage", ch02_02_custom_usage.app),
    ("03-optimize", ch02_03_optimize.app),
    ("04-optimize-cv", ch02_04_optimize_cv.app),
])
