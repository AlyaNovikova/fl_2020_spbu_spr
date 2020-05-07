import sys

from prettytable import PrettyTable

from CYK import cyk
from parser_def import parser, MyException
from utils import print_cf_grammar, print_tree, print_cf_grammar_in_normal_form
from chomsky_normal_form import normalize


if __name__ == '__main__':
    try:
        with open(sys.argv[1], 'r') as grammar_file:
            grammar = parser.parse(grammar_file.read())
    except MyException as exc:
        print(f'grammar parsing error: {exc}')
        sys.exit(1)

    print("Grammar parsed successfully!")
    print("Grammar:")
    print_cf_grammar(grammar)
    print('\n' * 3)

    grammar = normalize(grammar)
    print("Chomsky normal form:")
    print_cf_grammar_in_normal_form(grammar)
    print('\n' * 3)

    with open(sys.argv[2], 'r') as text_file:
        text = text_file.read()
    table, tree = cyk(grammar, text)

    if tree is None:
        print(f'"{text}" is NOT in grammar')
    else:
        print(f'"{text}" is in grammar!')
        print('Tree:')
        print_tree(tree, text)

        print('\n')
        print('Table:')
        t = PrettyTable([''] + [f'{i}' for i in range(len(text))])
        for i in range(len(text)):
            row = [f'{i}']
            for j in range(len(text)):
                row.append(','.join(table[i][j + 1]))
            t.add_row(row)
        print(t)