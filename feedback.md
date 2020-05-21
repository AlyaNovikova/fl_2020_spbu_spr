# Ваня (Олейник)
К сожалению терминалы - маленькие латинские буквы. 
Поэтому все служебные символы заменяем на их названия (получилась какая-то грамматика РУСОВ)

То, что можно писать в правилах `|` очень круто, плюсик к карме

Грамматика получилась такая:
```
!S >=> !RULES
!RULES >=> !RULE !PERENOS | !RULE !PERENOS !RULES
!RULE >=> !NONTERM !STRELKA !NECHTO | !NONTERM !STRELKA !NECHTO !ILI !RULE
!NECHTO >=> :!eps: | !TERM !NECHTO | !NONTERM !NECHTO
!NONTERM >=> !VOSKICAEM !BIGBYKVI
!TERM >=> !DVOETOCHIE !SMALLBYKVI !DVOETOCHIE
!BIGBYKVI >=> !BIGBYKVA !BIGBYKVIEPS
!BIGBYKVIEPS >=> :!eps: | !BIGBYKVA !BIGBYKVIEPS
!SMALLBYKVI >=> !SMALLBYKVA !SMALLBYKVIEPS
!SMALLBYKVIEPS >=> :!eps: | !SMALLBYKVA !SMALLBYKVIEPS
!BIGBYKVA >=> :a: :a: | :b: :b:| :c: :c:| :d: :d:| :e: :e:| :f: :f:
!BIGBYKVA >=> :g: :g: | :h: :h:| :i: :i:| :j: :j:| :k: :k:| :l: :l:
!BIGBYKVA >=> :m: :m: | :n: :n:| :o: :o:| :p: :p:| :q: :q:| :r: :r:
!BIGBYKVA >=> :s: :s: | :t: :t:| :u: :u:| :v: :v:| :w: :w:| :x: :x:
!BIGBYKVA >=> :y: :y: | :z: :z:
!SMALLBYKVA >=> :a: | :b: | :c: | :d: | :e: | :f:
!SMALLBYKVA >=> :g: | :h: | :i: | :j: | :k: | :l:
!SMALLBYKVA >=> :m: | :n: | :o: | :p: | :q: | :r:
!SMALLBYKVA >=> :s: | :t: | :u: | :v: | :w: | :x:
!SMALLBYKVA >=> :y: | :z:
!PERENOS >=> :p: :e: :r: :e: :n: :o: :s:
!STRELKA >=> :s: :t: :r: :e: :l: :k: :a:
!ILI >=> :i: :l: :i:

```


Успешно распарсилось!

Одобряю! 

# Егор (Горбачёв)
По документации видно, что автор не из Англии, так что тут
плюс за верность родине
 
Как и у Вани, здесь терминалы - только маленькие буквы. 
Но это ничего, после русов нам ничего не страшно!

Грамматика: 

```
S -> RULES
RULES -> RULE, PERENOS
RULES -> RULE, PERENOS, RULES
RULE -> NONTERM, STRELKA, NECHTO
NECHTO -> @
NECHTO -> TERM, NECHTO
NECHTO -> NONTERM, NECHTO
NONTERM -> BIGBYKVI
TERM -> SMALLBYKVI
BIGBYKVI -> BIGBYKVA, BIGBYKVIEPS
BIGBYKVIEPS -> @
BIGBYKVIEPS ->BIGBYKVA, BIGBYKVIEPS
SMALLBYKVI -> SMALLBYKVA, SMALLBYKVIEPS
SMALLBYKVIEPS -> @
SMALLBYKVIEPS -> SMALLBYKVA, SMALLBYKVIEPS
BIGBYKVA -> a, a
BIGBYKVA -> b, b
BIGBYKVA -> c, c
BIGBYKVA -> d, d
BIGBYKVA -> e, e
BIGBYKVA -> f, f
BIGBYKVA -> g, g
BIGBYKVA -> h, h
BIGBYKVA -> i, i
BIGBYKVA -> j, j
BIGBYKVA -> k, k
BIGBYKVA -> l, l
BIGBYKVA -> m, m
BIGBYKVA -> n, n
BIGBYKVA -> o, o
BIGBYKVA -> p, p
BIGBYKVA -> q, q
BIGBYKVA -> r, r
BIGBYKVA -> s, s
BIGBYKVA -> t, t
BIGBYKVA -> u, u
BIGBYKVA -> v, v
BIGBYKVA -> w, w
BIGBYKVA -> x, x
BIGBYKVA -> y, y
BIGBYKVA -> z, z
SMALLBYKVA -> a
SMALLBYKVA -> b
SMALLBYKVA -> c
SMALLBYKVA -> d
SMALLBYKVA -> e
SMALLBYKVA -> f
SMALLBYKVA -> g
SMALLBYKVA -> h
SMALLBYKVA -> i
SMALLBYKVA -> j
SMALLBYKVA -> k
SMALLBYKVA -> l
SMALLBYKVA -> m
SMALLBYKVA -> n
SMALLBYKVA -> o
SMALLBYKVA -> p
SMALLBYKVA -> q
SMALLBYKVA -> r
SMALLBYKVA -> s
SMALLBYKVA -> t
SMALLBYKVA -> u
SMALLBYKVA -> v
SMALLBYKVA -> w
SMALLBYKVA -> x
SMALLBYKVA -> y
SMALLBYKVA -> z
PERENOS -> p, e, r, e, n, o, s
STRELKA -> s, t, r, e, l, k, a

```

Успешно распарсилось!

Ставлю лайк! 