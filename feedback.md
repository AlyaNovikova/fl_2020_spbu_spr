# Ваня (Павлов)

Первая программа - проверка на простоту за квадрат 

```
fun prime(n) 
{ 
    please help 
    div1 := n-1; 
    flag := 0; 
    poka (div1>1) { 
        div2 := 2; help help help help help help help help help help me 
        poka (div2<=div1&&div1*div2<=n) { 
            esle (div1*div2==n) 
                then { 
                    flag := 1; 
                } 
                else {}; 
            div2 := div2+1; 
        }; 
        div1 := div1-1; 
    }; 
    return flag; 
}; 

{ 
    read(x); 
    print(prime(x)); 
}
```   

Вторая программа - ближайшая степень двойки к натуральному числу n, которая >= n

```
fun stepen~dvoiki(n)
{ 
    ans := 1; 
    poka (ans<n) {
        ans := ans*2;
    };
    return ans;
};

{
    read(x);
    print(stepen~dvoiki(x)); 
}"

```

И очень отбитая программа (я просто пыталась прочекать несколько функций сразу) - 
'вероятностая' (ха ха) проверка на простоту. 
3 функции с разными формулами для проверки 

```
fun first(p, div1)
{
    div1 := div1*div1+1;
    div2 := 2;
    flag := 0;
    poka (div1*div2<p) { 
        esle (div1*div2==p) 
            then { 
                flag := 1; 
            } 
            else {}; 
        div2 := div2+1; 
    };
    return flag; 
};

fun second(p, div1)
{
    div1 := div1+239;
    div2 := 2;
    flag := 0;
    poka (div1*div2<p) { 
        esle (div1*div2==p) 
            then { 
                flag := 1; 
            } 
            else {}; 
        div2 := div2+1; 
    };
    return flag; 
};

fun third(p, div1)
{
    div1 := (div1-3)*(div1+10);
    div2 := 2;
    flag := 0;
    poka (div1*div2<p) { 
        esle (div1*div2==p) 
            then { 
                flag := 1; 
            } 
            else {}; 
        div2 := div2+1; 
    };
    return flag; 
};

{
    read(x);
    print(first(x,1)||second(x,1)||third(x,1));
}
```

Функции добавлены очень понятно, баги в коде искать легко (особенно в сравнении со следующим языком))0)))0

Все пробелы, скобки и точки с запятой проставлены логично, по аналогии с остальным кодом


В прошлый раз я вроде забыла отметить тильдочку в разделение переменных 

Выглядит качественно и приметно. Сразу видно, что к оформлению языка подошли с умом

Особое спасибо хочется выразить автору за то, что в языке есть пробельные символы 

С ними оказывается жизнь так прекрасна!  
 
      
# Андрей (Древний Рус)

(Потратила на этот фидбек 1.5 часа. Андрей - за что??!?!?!?!?)

Документация конечно хромает (или вернее лежит в больнице со сломаной ногой), но язык незабываемый!

Хочется от всего сердца поблагодарить автора за неизгладимые впечатления!

Очевидным плюсом является то, что этот язык вы запомните навсегда

Особое спасибо за _VIDDAI_ . Я конечно сразу распознала, что две D тут не просто так. Плюсик за отсылку

Да и вообще, что тут ещё можно сказать. Функции добавлены очень по-древнерусски. 
Всё соблюдено в лучших традициях

А все минусы данного языка можно списать на его древность (что уж поделать, если раньше никто не слыхал о пробелах)

Так что автору огромное спасибо! Остались только положительные эмоции

  


(пробелы в моих примерах для удобства чтения (АХАХАХАХАХХА))

1. Ближайшая степень двойки к натуральному числу r, которая >= r

```
~SHUE_PPSH~
{
#VIZNACH#_rUUU_(@r@)

{#ROBIT#
{#ZVYAZATI#@ruS@:$CELKOVIY$:}
{#PAKUL#:@ruS@<@r@:{#ZVYAZATI#@ruS@:@ruS@*$POLUSHKA$:}}}
{#VIDDAI#:@ruS@:}
}

{#ROBIT#{#CHITATSBERESTI#@rus@}{#NAPISATNABERESTU#:_rUUU_(@rus@):}}
```

2. Факториал (ухххххх, дебажила около 40 минут. Оказалось, что это была потеряная {

```
~SHUE_PPSH~
{
#VIZNACH#_rUUU_(@r@)
    {#ROBIT#{#ZVYAZATI#@ruS@:$CELKOVIY$:}{#PAKUL#:@r@>$NOL$:{#ROBIT#{#ZVYAZATI#@ruS@:@ruS@*@r@:}{#ZVYAZATI#@r@:@r@-$CELKOVIY$:}}}}
    {#VIDDAI#:@ruS@:}
}

{#ROBIT#{#CHITATSBERESTI#@rus@}{#NAPISATNABERESTU#:_rUUU_(@rus@):}}
```


3. Давайте посчитаем сумму от 1 до r и вернём 8 :)
```
~SHUE_PPSH~
{
#VIZNACH#_rUUU_(@r@)
    {#ROBIT#{#ZVYAZATI#@ruS@:$NOL$:}{#PAKUL#:@r@>$NOL$:{#ROBIT#{#ZVYAZATI#@ruS@:@ruS@+@r@:}{#ZVYAZATI#@r@:@r@-$CELKOVIY$:}}}}
    {#VIDDAI#:$ZOLOTNICHOK$:}
}

{#ROBIT#{#CHITATSBERESTI#@rus@}{#NAPISATNABERESTU#:_rUUU_(@rus@):}}
```