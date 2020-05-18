from dataclasses import dataclass

@dataclass
class Context:
    total_size: int
    batch_size: int
    n_baches: int
    batch_index: int
