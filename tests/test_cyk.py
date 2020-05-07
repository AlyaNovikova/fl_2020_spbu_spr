from CYK import cyk
from chomsky_normal_form import normalize
from parser_def import parser


def test_psp():
    grammar = """@A :=
        @A := @A + "(" + @A + ")"
        """

    assert cyk(normalize(parser.parse(grammar)), "") is not None
    assert cyk(normalize(parser.parse(grammar)), "()") is not None
    assert cyk(normalize(parser.parse(grammar)), "(())") is not None
    assert cyk(normalize(parser.parse(grammar)), "()()") is not None
    assert cyk(normalize(parser.parse(grammar)), "()(())") is not None
    assert cyk(normalize(parser.parse(grammar)), "()()((())())") is not None

    assert cyk(normalize(parser.parse(grammar)), "(") is None
    assert cyk(normalize(parser.parse(grammar)), "())") is None
    assert cyk(normalize(parser.parse(grammar)), "()(()") is None
    assert cyk(normalize(parser.parse(grammar)), "(()(()") is None
    assert cyk(normalize(parser.parse(grammar)), "(((((())") is None
