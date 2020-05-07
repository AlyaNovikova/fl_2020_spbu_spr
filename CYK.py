def cyk(rules, text: str):

    if len(text) == 0:
        for rule in rules:
            if len(rule.seq) == 0:
                return rule
        assert False

    m = [[[] for _ in range(len(text) + 1)] for _ in range(len(text) + 1)]
    prev = [[[] for _ in range(len(text) + 1)] for _ in range(len(text) + 1)]

    for rule in rules:
        if len(rule.seq) == 1:
            symbol = rule.seq[0]
            for i in range(len(text)):
                if text[i] == symbol:
                    m[i][i + 1].append(rule.nonterm)
                    prev[i][i + 1].append(rule)

    for l in range(2, len(text) + 1):
        for i in range(len(text) - l + 1):
            j = i + l
            for k in range(i + 1, j):
                for rule in rules:
                    if len(rule.seq) != 2:
                        continue
                    a, b = rule.seq
                    if a in m[i][k] and b in m[k][j]:
                        ai = m[i][k].index(a)
                        bi = m[k][j].index(b)
                        m[i][j].append(rule.nonterm)
                        prev[i][j].append((rule, prev[i][k][ai], prev[k][j][bi]))

    if '@-1' in m[0][len(text)]:
        return prev[0][len(text)][m[0][len(text)].index('@-1')]
    else:
        return None