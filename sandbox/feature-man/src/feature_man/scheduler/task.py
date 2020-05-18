from abc import ABC, abstractmethod
from typing import Optional, Type

from ..context import Context
from ..exceptions import ExceedMaxBatchSize
from ..ops import BaseOp
from .data_store import DataStore


class BaseTask(ABC):
    def __init__(self, op: Type[BaseOp], max_batch_size: Optional[int] = None) -> None:
        if max_batch_size is not None and max_batch_size <= 1:
            raise ValueError(f"invalid max_batch_size was given. {max_batch_size}")

        self._op = op
        self.max_batch_size = max_batch_size

    def __call__(
        self, context: Context, input_store: DataStore, output_store: DataStore
    ) -> None:
        if self.max_batch_size is not None and self.max_batch_size < len(input_store):
            raise ExceedMaxBatchSize(
                f"store size exceeds max_batch_size: {self.max_batch_size} < {len(input_store)}"
            )

        self._run(context, input_store, output_store)

    @abstractmethod
    def _run(
        self, context: Context, input_store: DataStore, output_store: DataStore
    ) -> None:
        ...


class MapTask(BaseTask):
    def __init__(self, op: Type[BaseOp], max_batch_size: Optional[int] = None) -> None:
        super().__init__(op, max_batch_size)

    def _run(
        self, context: Context, input_store: DataStore, output_store: DataStore
    ) -> None:
        for data in input_store:
            result = self._op(context, [data])[0]
            output_store.append(result)


class ReduceTask(BaseTask):
    def __init__(self, op: Type[BaseOp], max_batch_size: Optional[int] = None) -> None:
        super().__init__(op, max_batch_size)

    def _run(
        self, context: Context, input_store: DataStore, output_store: DataStore
    ) -> None:
        results = self._op(context, list(input_store))
        for r in results:
            output_store.append(r)
