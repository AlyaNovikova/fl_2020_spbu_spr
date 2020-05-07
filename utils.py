def is_nonterminal(t):
    return t[0] == '@'


def print_cf_grammar(cf):
    all_nonterms = get_nonterms(cf)

    print(f'all nonterms: {", ".join(all_nonterms)}')
    print(f'start nonterm: {cf[0].nonterm}')
    print('rules:')
    for rule in cf:
        print(f'\t{rule.nonterm} -> {" ".join(rule.seq)}')


def print_cf_grammar_in_normal_form(cf):
    all_nonterms = get_nonterms(cf)

    print(f'all nonterms: {", ".join(all_nonterms)}')
    print(f'start nonterm: @-1')
    print('rules:')
    for rule in cf:
        print(f'\t{rule.nonterm} -> {" ".join(rule.seq)}')


def get_nonterms(cf):
    left = {rule.nonterm for rule in cf}
    right = {elem for rule in cf for elem in rule.seq if is_nonterminal(elem)}
    right.update(left)
    return right

def dfs_to_print_tree(v, text_iter, level):
    if type(v) is tuple:
        rule, left, right = v
        dfs_to_print_tree(right, text_iter, level + 1)
        print(f'{"            " * level}{rule.nonterm} -> {rule.seq[0]} {rule.seq[1]}')
        dfs_to_print_tree(left, text_iter, level + 1)
    else:
        print(f'{"            " * level}{v.nonterm:3s} -> {v.seq}')


def print_tree(tree, text):
    text_iter = iter(text)
    dfs_to_print_tree(tree, text_iter, 0)