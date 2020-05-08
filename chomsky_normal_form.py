import itertools

from grammer_ast import Rule
from utils import is_nonterminal, print_cf_grammar


def new_name(n):
    return f'@{n}'


def split_strings(rules):
    new_rules = []
    for rule in rules:
        seq = []
        for t in rule.seq:
            if is_nonterminal(t):
                seq.append(t)
            else:
                seq.extend(t)
        new_rules.append(Rule(rule.nonterm, seq))
    return new_rules


def first_step(rules, counter):
    new_rules = []
    for rule in rules:
        if len(rule.seq) > 1:
            new_seq = []
            for symbol in rule.seq:
                if not is_nonterminal(symbol):
                    new_nonterm = new_name(next(counter))
                    new_rules.append(Rule(new_nonterm, [symbol]))
                    new_seq.append(new_nonterm)
                else:
                    new_seq.append(symbol)
            new_rules.append(Rule(rule.nonterm, new_seq))
        else:
            new_rules.append(rule)
    return new_rules


def long_rule(rules, counter):
    new_rules = []
    for rule in rules:
        if len(rule.seq) > 2:
            prev_term = rule.nonterm
            for i in range(len(rule.seq) - 2):
                new_nonterm = new_name(next(counter))
                new_rules.append(Rule(prev_term, [rule.seq[i], new_nonterm]))
                prev_term = new_nonterm
            new_rules.append(Rule(prev_term, rule.seq[-2:]))
        else:
            new_rules.append(rule)
    return new_rules


def remove_eps(rules, start_nonterm):
    eps = set()
    for rule in rules:
        if len(rule.seq) == 0:
            eps.add(rule.nonterm)

    flag = True
    while flag:
        flag = False
        for rule in rules:
            if rule.nonterm not in eps and all(nt in eps for nt in rule.seq):
                eps.add(rule.nonterm)
                flag = True

    new_rules = []
    for rule in rules:
        if len(rule.seq) == 0 and rule.nonterm != start_nonterm:
            continue

        if len(rule.seq) == 2:
            if rule.seq[0] in eps:
                new_rules.append(Rule(rule.nonterm, [rule.seq[1]]))
            if rule.seq[1] in eps:
                new_rules.append(Rule(rule.nonterm, [rule.seq[0]]))

        new_rules.append(rule)

    if start_nonterm in eps:
        new_rules.append(Rule(start_nonterm, []))
    return new_rules


def new_start(rules, start_nonterm):
    new_rules = [Rule(new_name(-1), [start_nonterm])]

    for rule in rules:
        if rule.nonterm == start_nonterm and len(rule.seq) == 0:
            new_rules.append(Rule(new_name(-1), []))
        else:
            new_rules.append(rule)

    return new_rules


def remove_unary(rules):
    rules = set(rules)

    while True:
        to_add = set()

        for rule in rules:
            if len(rule.seq) == 1:
                for other in rules:
                    if rule.seq[0] == other.nonterm:
                        new_rule = Rule(rule.nonterm, other.seq)
                        if new_rule not in rules:
                            to_add.add(new_rule)

        if len(to_add) == 0:
            break

        rules.update(to_add)

    return [rule for rule in rules if len(rule.seq) != 1 or not is_nonterminal(rule.seq[0])]


def normalize(rules):
    start_nonterm = rules[0].nonterm
    counter = itertools.count()

    rules = split_strings(rules)
    rules = first_step(rules, counter)
    rules = long_rule(rules, counter)
    rules = remove_eps(rules, start_nonterm)
    rules = new_start(rules, start_nonterm)
    rules = remove_unary(rules)

    return rules
