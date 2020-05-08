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


def test_long_rules():
    grammar = """@S := 
    @S := "a" + "b" + "c" + @D + @E
    @D := "d" + "d" + "d" + "d"
    @E := "eeee" 
    @E := 
    """

    assert cyk(normalize(parser.parse(grammar)), "") is not None
    assert cyk(normalize(parser.parse(grammar)), "abcdddd") is not None
    assert cyk(normalize(parser.parse(grammar)), "abcddddeeee") is not None

    assert cyk(normalize(parser.parse(grammar)), "a") is None
    assert cyk(normalize(parser.parse(grammar)), "abc") is None
    assert cyk(normalize(parser.parse(grammar)), "abcddd") is None
    assert cyk(normalize(parser.parse(grammar)), "abcddddeee") is None
    assert cyk(normalize(parser.parse(grammar)), "abceeee") is None
    assert cyk(normalize(parser.parse(grammar)), "eeee") is None
    assert cyk(normalize(parser.parse(grammar)), "ddddeeee") is None


def test_long_chain():
    grammar = """@S := @A 
    @A := @B 
    @B := "b"
    @B := @C 
    @C := 
    @C := @D
    @D := @E
    @E := 
    @E := "e" 
    """

    assert cyk(normalize(parser.parse(grammar)), "") is not None
    assert cyk(normalize(parser.parse(grammar)), "b") is not None
    assert cyk(normalize(parser.parse(grammar)), "e") is not None

    assert cyk(normalize(parser.parse(grammar)), "be") is None
    assert cyk(normalize(parser.parse(grammar)), "bb") is None
    assert cyk(normalize(parser.parse(grammar)), "ee") is None
    assert cyk(normalize(parser.parse(grammar)), "b ") is None
    assert cyk(normalize(parser.parse(grammar)), "e ") is None
    assert cyk(normalize(parser.parse(grammar)), " ") is None