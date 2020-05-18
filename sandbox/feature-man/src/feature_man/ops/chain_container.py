from typing import Dict, List, Optional, Sequence, Type, Union

import networkx as nx
import numpy as np

from ..context import Context
from ..types import Shape
from .base import BaseOp
from .node import Node, create_concrete_node


class ChainContainer(BaseOp):
    ROOT_NODE_NAME: str = "root"

    @property
    def input_shapes(self) -> Union[Shape, List[Shape]]:
        return self._input_shapes

    @property
    def output_shape(self) -> Optional[Shape]:
        return self._output_shape

    def __init__(
        self,
        ops_or_nodes: Sequence[Union[Node, Type[BaseOp]]],
        input_shapes: Optional[Union[Shape, List[Shape]]] = None,
        output_shape: Optional[Shape] = None,
    ):
        if len(ops_or_nodes) == 0:
            raise ValueError("empty ops_or_nodes sequence was given")

        self._nodes = self.create_nodes(ops_or_nodes)
        self._proc_order = self.calc_proc_order(self._nodes)

        head_op = self._nodes[self._proc_order[0]].op
        self._input_shapes = (
            head_op.input_shapes if input_shapes is None else input_shapes
        )
        self._output_shape = (
            head_op.output_shape if output_shape is None else output_shape
        )

    def _compute_feature(
        self, context: Context, arrays: List[np.ndarray]
    ) -> List[np.ndarray]:
        outputs = {self.ROOT_NODE_NAME: arrays}
        for node_name in self._proc_order:
            node = self._nodes[node_name]
            args = [outputs[s] for s in node.source]
            outputs[node.name] = node.op(context, *args)
        return outputs[self._proc_order[-1]]

    @classmethod
    def create_nodes(
        cls, ops_or_nodes: Sequence[Union[Node, Type[BaseOp]]]
    ) -> Dict[str, Node]:
        result = {}
        last_node_name = cls.ROOT_NODE_NAME
        for e in ops_or_nodes:
            node = create_concrete_node(e, last_node_name)
            result[node.name] = node
            last_node_name = node.name
        return result

    @classmethod
    def calc_proc_order(cls, nodes: Dict[str, Node]) -> List[str]:
        dag = []
        for n in nodes.values():
            for s in n.source:
                dag.append((s, n.name))
        return list(nx.topological_sort(nx.DiGraph(dag)))[1:]  # Remove root node
