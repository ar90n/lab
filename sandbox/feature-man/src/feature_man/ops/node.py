import random
import string
from dataclasses import dataclass
from typing import Optional, Tuple, Type, Union, cast

from .base import BaseOp


@dataclass
class Node:
    op: Type[BaseOp]
    name: Optional[str] = None
    source: Optional[Tuple[str]] = None


@dataclass
class ConcreteNode:
    op: Type[BaseOp]
    name: str
    source: Tuple[str]


def create_node_name() -> str:
    characters = string.ascii_letters + string.digits
    return "".join((random.choice(characters) for _ in range(12)))


def create_concrete_node(
    src: Union[Node, Type[BaseOp]], last_node_name: str
) -> ConcreteNode:
    if isinstance(src, BaseOp):
        return ConcreteNode(
            cast(Type[BaseOp], src), create_node_name(), (last_node_name,)
        )
    elif isinstance(src, Node):
        name = create_node_name() if src.name is None else src.name
        source = (last_node_name,) if src.source is None else src.source
        return ConcreteNode(src.op, name, source)
    raise TypeError("not op or node was given")
