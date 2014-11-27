# takuzu-erl

Программа генерации поля для игры [takuzu](http://en.wikipedia.org/wiki/Takuzu).

## Использование

Для работы требуется установленный Erlang.

```
$ git clone https://github.com/dryewo/takuzu-erl.git && cd $_
$ erl
1> c(takuzu, {d, 'TEST'}), takuzu:test().
  All 15 tests passed.
ok

2> timer:tc(fun()->takuzu:generate(8) end).
Generation done, O:12, X:9
X O . O . . . . 
O . . O . X . . 
. O O . . . . . 
. . O . . . X O е
. . . . O . X . 
O . X . O . . . 
. . . O . . . . 
. X X . X . . X 

{152386,ok}
```

## Правила takuzu

1. Не может быть более двух одинаковых клеток рядом.
2. В каждой строке или столбце должно быть одинаковое количество X и O.
3. Не должно быть одинаковых строк или одинаковых столбцов.
4. Размер поля — четное число >= 4.

## License

Copyright © 2014 Dmitry Balakhonskiy