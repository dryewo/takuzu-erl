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
. . O . . . X O 
. . . . O . X . 
O . X . O . . . 
. . . O . . . . 
. X X . X . . X 

{152386,ok}
```

## License

Copyright © 2014 Dmitry Balakhonskiy