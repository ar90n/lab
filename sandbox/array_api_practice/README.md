# array_api_practice

## result
```bash
$ ARRAY_API_MODULE=numpy.array_api python main.py
/usr/local/lib/python3.8/importlib/__init__.py:127: UserWarning: The numpy.array_api submodule is still experimental. See NEP 47.
  return _bootstrap._gcd_import(name[level:], package, level)
[[1. 0. 0.]
 [0. 1. 0.]
 [0. 0. 1.]]
[0 1 2]
[[1 2 3]
 [4 5 6]]
[[1 4]
 [2 5]
 [3 6]]
2
(2, 3)
$ ARRAY_API_MODULE=torch python main.py
tensor([[1., 0., 0.],
        [0., 1., 0.],
        [0., 0., 1.]])
tensor([0, 1, 2])
tensor([[1, 2, 3],
        [4, 5, 6]])
tensor([[1, 4],
        [2, 5],
        [3, 6]])
2
torch.Size([2, 3])
```