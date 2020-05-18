from __future__ import annotations

import pickle
from dataclasses import dataclass
from itertools import accumulate
from pathlib import Path
from typing import Any, Callable, Iterable, Iterator, List, Optional, Union
from urllib.parse import urlparse

import cv2
import numpy as np
from smart_open import open


@dataclass
class Entity:
    value: Any
    uri: Optional[Union[str, Path]] = None


def _create_initial_entity(data: Any) -> Entity:
    if isinstance(data, Entity):
        return data
    elif isinstance(data, Path):
        return Entity(None, data)
    elif isinstance(data, str):
        try:
            urlparse(data)
            return Entity(None, data)
        except ValueError:
            pass
    return Entity(data)


def _init_store(initial_data: Iterable[Any]) -> List[Entity]:
    return [_create_initial_entity(v) for v in initial_data]


def _decode_bytes(bytes: bytes) -> np.ndarray:
    try:
        return pickle.loads(bytes)
    except pickle.UnpicklingError:
        pass

    array_u8 = np.fromstring(bytes, np.uint8)
    try:
        # transpose to (C, H, W) order
        return cv2.imdecode(array_u8, cv2.IMREAD_COLOR).transpose([2, 0, 1])
    except Exception:
        pass

    return array_u8


class DataStore(Iterable):
    def __init__(
        self,
        initial_data: Iterable[Any],
        entity_builder: Callable[..., Entity] = lambda data: Entity(data),
        use_cache: bool = True,
    ):
        self._store = _init_store(initial_data)
        self._entity_builder = entity_builder
        self.use_cache = use_cache

    def at(self, index: int) -> Any:
        entity = self._store[index]

        result = entity.value
        if result is None and entity.uri is not None:
            with open(entity.uri, "rb") as f:
                bytes = f.read()
            result = _decode_bytes(bytes)

        if self.use_cache:
            entity.value = result
        return result

    def append(self, data: Any) -> None:
        entity = self._entity_builder(data)

        if entity.uri is not None:
            with open(entity.uri, "wb") as f:
                pickle.dump(data, f)

        if not self.use_cache:
            entity.value = None
        self._store.append(entity)

    def split(self, max_size:int) -> List[DataStore]:
        n_children = (len(self) + max_size - 1) // max_size
        size = len(self) // n_children
        rem_stores = len(self) % n_children
        l = [0] + list(accumulate([size + 1] * rem_stores + [size] * (n_children - rem_stores)))

        res = []
        for b, e in zip(l, l[1:]):
            res.append(DataStore(self._store[b:e], self._entity_builder, self.use_cache))
        return res


    def purge_cache(self) -> None:
        for v in self._store:
            if v.uri is not None:
                v.value = None

    def __iter__(self) -> Iterator:
        return (self.at(i) for i in range(len(self)))

    def __len__(self) -> int:
        return len(self._store)
