import sys

from parser_def import parser


def print_cf_grammar(cf):
    all_nonterms = {elem for rule in cf for elem in rule.seq if elem.startswith('@')}
    all_nonterms.update(rule.nonterm for rule in cf)

    print(f'all nonterms: {", ".join(all_nonterms)}')
    print('rules:')
    for rule in cf:
        print(f'{rule.nonterm} -> {" ".join(rule.seq)}')


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as inp_file:
        grammar = parser.parse(inp_file.read())
    print_cf_grammar(grammar)
