from abc import ABC, abstractmethod
from pathlib import Path
from typing import Iterable, List, Type, Union

import numpy as np

from .data_store import DataStore
from .task import BaseTask


class BaseScheduler(ABC):
    def __init__(self, tasks: Iterable[Type[BaseTask]]) -> None:
        self._tasks = list(tasks)

    def __call__(self, input_paths: List[Union[str, Path]]) -> List[np.ndarray]:
        input_store = DataStore(input_paths)
        output_store = self._run(input_store)
        return list(output_store)

    @classmethod
    def _get_batch_size(cls, task: Type[BaseTask], store: DataStore) -> int:
        return len(store) if task.max_batch_size is None else task.max_batch_size

    @abstractmethod
    def _run(self, input_store: DataStore) -> DataStore:
        ...
