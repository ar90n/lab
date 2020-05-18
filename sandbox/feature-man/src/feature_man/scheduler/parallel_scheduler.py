from typing import Iterable, Type

import ray

from ..context import Context
from .base_scheduler import BaseScheduler
from .data_store import DataStore
from .task import BaseTask


@ray.remote
def _apply(
    task: Type[BaseTask],
    context: Context,
    input_store: DataStore,
    output_store: DataStore,
):
    task(context, input_store, output_store)
    return output_store


def _merge_data_stores(stores: Iterable[DataStore]) -> DataStore:
    return DataStore(sum([list(s) for s in stores], []))


class ParallelScheduler(BaseScheduler):
    def __init__(self, tasks: Iterable[Type[BaseTask]]) -> None:
        super().__init__(tasks)
        if not ray.is_initialized():
            ray.init(num_cpus=4)

    def _run(self, input_store: DataStore) -> DataStore:
        for t in self._tasks:
            batch_size = self._get_batch_size(t, input_store)
            chunk_stores = input_store.split(batch_size)

            result_stores = ray.get(
                [
                    _apply.remote(
                        t,
                        Context(len(input_store), batch_size, len(chunk_stores), i),
                        chunk_store,
                        DataStore([]),
                    )
                    for i, chunk_store in enumerate(chunk_stores)
                ]
            )
            input_store = _merge_data_stores(result_stores)
        return input_store
