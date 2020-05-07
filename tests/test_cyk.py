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


def test_simple():
    grammar = """@S := @A
        @S := @B + "kokoko"
        @B := 
        @A := "a"
        @S := @C
        @S := @S
        """

    assert cyk(normalize(parser.parse(grammar)), "a") is not None
    assert cyk(normalize(parser.parse(grammar)), "kokoko") is not None

    assert cyk(normalize(parser.parse(grammar)), "") is None
    assert cyk(normalize(parser.parse(grammar)), "a ") is None
    assert cyk(normalize(parser.parse(grammar)), " ") is None
    assert cyk(normalize(parser.parse(grammar)), "b") is None
    assert cyk(normalize(parser.parse(grammar)), "kokok") is None
    assert cyk(normalize(parser.parse(grammar)), "k") is None


def test_circle():
    grammar = """@S := @S + @A
        @A := @A + @S
        @A := 
        @S := "s"
        """

    assert cyk(normalize(parser.parse(grammar)), "s") is not None
    assert cyk(normalize(parser.parse(grammar)), "ss") is not None
    assert cyk(normalize(parser.parse(grammar)), "sss") is not None
    assert cyk(normalize(parser.parse(grammar)), "ssss") is not None
    assert cyk(normalize(parser.parse(grammar)), "sssssssssss") is not None

    assert cyk(normalize(parser.parse(grammar)), "") is None
    assert cyk(normalize(parser.parse(grammar)), "s ") is None
    assert cyk(normalize(parser.parse(grammar)), "a") is None
    assert cyk(normalize(parser.parse(grammar)), " ") is None
