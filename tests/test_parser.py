import pytest

from grammer_ast import Rule
from parser_def import parser, MyException


def test_rule_eps():
    data = """@aba :=\n"""
    cf = parser.parse(data)
    assert cf == [Rule('@aba', [])]


def test_rule_simple_terminal_string():
    data = """@aba := "terminals"\n"""
    cf = parser.parse(data)
    assert cf == [Rule('@aba', ["terminals"])]


def test_rule_simple_terminal_string_inner_quotes():
    data = """@aba := "\\"term\\"inals"\n"""
    cf = parser.parse(data)
    assert cf == [Rule('@aba', [r'\"term\"inals'])]


def test_rule_full():
    data = """@aba := @a + "b" + @a\n"""
    cf = parser.parse(data)
    assert cf == [Rule('@aba', ["@a", 'b', "@a"])]


def test_rules():
    data = """@aba := @a + "b" + @a
    @caba := "c" + @aba
    """
    cf = parser.parse(data)
    assert cf == [Rule('@aba', ["@a", 'b', "@a"]), Rule('@caba', ['c', '@aba'])]


def test_lexer():
    with pytest.raises(MyException):
        parser.parse('@A = "a"\n')


def test_parser():
    with pytest.raises(MyException):
        parser.parse('@A := := "a"\n')


