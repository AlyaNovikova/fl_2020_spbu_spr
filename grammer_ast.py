class Rule:
    def __init__(self, nonterm, seq):
        self.nonterm = nonterm
        self.seq = seq

    def __eq__(self, other):
        return self.nonterm == other.nonterm and self.seq == other.seq

    def __hash__(self):
        return hash((self.nonterm, tuple(self.seq)))
