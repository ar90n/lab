from typing import Iterable, Type

from ..context import Context
from .base_scheduler import BaseScheduler
from .data_store import DataStore
from .task import BaseTask


class SimpleScheduler(BaseScheduler):
    def __init__(self, tasks: Iterable[Type[BaseTask]]) -> None:
        super().__init__(tasks)

    def _run(self, input_store: DataStore) -> DataStore:
        for t in self._tasks:
            output_store = DataStore([])
            batch_size = self._get_batch_size(t, input_store)
            chunk_stores = input_store.split(batch_size)
            for i, chunk_store in enumerate(chunk_stores):
                context = Context(len(input_store), len(chunk_store), len(chunk_stores), i)
                t(context, chunk_store, output_store)
            input_store = output_store
        return input_store
