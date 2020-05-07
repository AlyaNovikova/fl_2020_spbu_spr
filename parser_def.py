import sys

from ply import lex, yacc

from grammer_ast import Rule


class MyException(Exception):
    pass


tokens = (
    'NONTERMINAL',
    'ASSIGN',
    'PLUS',
    'TERMINAL_STRING',
    'NEWLINE',
)

t_NONTERMINAL = r'@[a-zA-Z]+'
t_ASSIGN = r':='
t_PLUS = r'\+'
t_TERMINAL_STRING = r'"([a-zA-Z0-9:=\+\-\*/\(\)]|\\")*"'
t_NEWLINE = r'\n'

t_ignore = ' \t'


def t_error(t):
    raise MyException(f'Illegal character {t.value[0]!r} in line {t.lineno} (position {t.lexpos} from the beginning of the file, lol)')


lexer = lex.lex()


def p_cf_rule(p):
    'cf : rule'
    p[0] = [p[1]]


def p_cf_rules(p):
    'cf : cf rule '
    p[0] = p[1] + [p[2]]


def p_rule_empty(p):
    'rule : NONTERMINAL ASSIGN NEWLINE'
    p[0] = Rule(p[1], [])


def p_rule_seq(p):
    'rule : NONTERMINAL ASSIGN seq NEWLINE'
    p[0] = Rule(p[1], p[3])


def p_seq_nonterminal(p):
    'seq : NONTERMINAL'
    p[0] = [p[1]]


def p_seq_terminal(p):
    'seq : TERMINAL_STRING'
    p[0] = [p[1][1:-1]]


def p_seq_plus(p):
    'seq : seq PLUS seq'
    p[0] = p[1] + p[3]


def p_error(p):
    raise MyException(f'unexpected token')


parser = yacc.yacc()
