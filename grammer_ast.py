from typing import List
from dataclasses import dataclass


@dataclass
class Rule:
    nonterm: str
    seq: List[str]

    def __hash__(self):
        return hash((self.nonterm, tuple(self.seq)))
