from typing import List
from dataclasses import dataclass


@dataclass
class Rule:
    nonterm: str
    seq: List[str]
