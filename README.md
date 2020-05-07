# Запуск
Я писала для python 3.8, но скорее всего весь 3 питон будет работать 


2 задание:
Команда для запуска парсера грамматики -  

`python main.py input_file`

3 задание:
Ждёмс


Тесты:
Для запуска тестов - 

`pytest .`


Библиотеки:
Установить нужные библиотеки - 

`pip install ply pytest`

#  Cинтаксис (1 задание)
Нетерминалы - строки из латинских букв в любом регистре, с обязательной 'собакой' вначале:

```
@aBA
@BEbeBE
@S
```

Строки терминалов - это строки в кавычках, состоящие из символов 
```
[a-z], [A-Z], [0-9],
':', '=', '+', '-', '*', '/', '(', ')'
"\"" 
```
последнее нужно, чтобы написать кавычку внутри терминала 
(кавычка внутри терминала пишется с обязательным символом '\\' перед ней)


Примеры: 
```
"+"
"(privet)"
"\"term\"inals"
```


Если кому-нибудь понадобятся другие символы - пишите, добавим :)

Правила представляют собой строку вида:
```
NonTerminal := A1 + A2 + ... + An
```
где 

NonTerminal - это нетерминал
 
Ai - либо нетерминал, либо строка терминалов 


Пример: 
```
@aba := "(" + @b + "kek" + @aba + ")"
@eps := 
```
(если ничего нету после ":=", то это пустое правило)


Вся грамматика представляет собой набор правил

Все правила находятся в разных строках, то есть отделены символом '\n'
**В конце (в последнем правиле) тоже должен быть перенос строки**


Символы табуляции и пробелы игнорируются

Стартовым нетерминалом считается первое правило