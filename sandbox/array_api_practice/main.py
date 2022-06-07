import os
from importlib import import_module
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    import numpy.array_api as xp
else:
    mod_name = os.environ.get("ARRAY_API_MODULE", "numpy.array_api")
    # follwoing codes from https://github.com/data-apis/array-api-tests/blob/master/array_api_tests/_array_module.py
    _module, _sub = mod_name, None
    if "." in mod_name:
        _module, _sub = mod_name.split(".", 1)
    xp = import_module(_module)
    if _sub:
        try:
            xp = getattr(xp, _sub)
        except AttributeError:
            # _sub may be a submodule that needs to be imported. WE can't
            # do this in every case because some array modules are not
            # submodules that can be imported (like mxnet.nd).
            xp = import_module(mod_name)

a = xp.eye(3, 3)
print(a)
b = xp.arange(3)
print(b)
c = xp.asarray([[1, 2, 3], [4, 5, 6]])
print(c)
print(c.T)
print(c.ndim)
print(c.shape)