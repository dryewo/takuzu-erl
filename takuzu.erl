%%%-------------------------------------------------------------------
%%% @author dbalakhonsky
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. ноя 2014 19:31
%%%-------------------------------------------------------------------
-module(takuzu).
-author("dbalakhonsky").

%% API
-export([generate/1, generate/2, generate_print/2, measure/2]).
-compile([export_all]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Публичные функции
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Генерирует поле путем постепенного заполнения клеток рандомом и пытаясь решить каждую итерацию
generate(Size) -> generate(Size, true).
generate(Size, Parallel) ->
  Initial = takuzu:new(Size),
  generate_solvable(Initial, Parallel).

generate_print(Size) -> generate_print(Size, true).
generate_print(Size, Parallel) ->
  {Time, {ok, Res}} = timer:tc(fun() -> generate(Size, Parallel) end),
  {Ones, Twos} = {count_elements(Res, 1), count_elements(Res, 2)},
  io:format("Generation done, O:~p, X:~p~n", [Ones, Twos]),
  print_field(Res),
  io:format("~p ms~n", [Time / 1000]).

%% Count раз вызывает функцию F/0, засекая время, возвращает статистику
measure(F, Count) ->
  {TotalTime, Times} = (timer:tc(fun() ->
    lists:map(fun(_) -> {MSec, _} = timer:tc(fun() -> F() end), MSec end, lists:seq(1, Count)) end)),
%%   io:format("~p~n", [Times]),
  {TotalTime / 1000, stats(Times)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Алгоритм генерации
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_solvable(Field, Parallel) ->
  %% Пробуем решить что дали
  {SolveRes, [First | _]} = try_solve(Field, Parallel),
  case SolveRes of
    ok -> {ok, reduce_solvable(First, Parallel)}; %% Если решилось, это вин
    _ -> case random_step_checked(Field, Parallel) of %% Если не решилось, заполняем случайную клетку допустимым образом
           {ok, NextGen} ->
%%              print_field(Field),
             generate_solvable(NextGen, Parallel) %% Если заполнилось, снова пытаемся решить
%%           _ -> {impossibru, Field} %% Если не заполнилось, тогда ой, этого вообще быть не может
         end
  end.

%% Пытается решить заданное поле
try_solve(Field, Parallel) -> try_solve_2([Field], Parallel).
try_solve_2([LastStep | Steps], Parallel) ->
  case is_solved(LastStep) of
    true -> {ok, lists:reverse([LastStep | Steps])};
    _ ->
      Suggestions = check_field(LastStep, Parallel),
      case any_suggestions(Suggestions) of
        true ->
          NextStep = apply_suggestions(LastStep, Suggestions),
          try_solve_2([NextStep, LastStep | Steps], Parallel);
        _ -> {impossibru, lists:reverse([LastStep | Steps])}
      end
  end.

is_solved(Field) -> lists:all(fun(I) -> I /= 0 end, Field).

any_suggestions(Suggestions) -> lists:any(fun({_, _, _}) -> true; (_) -> false end, Suggestions).

%% Применяет к полю указанные предложения
apply_suggestions(Field, Suggestions) ->
  [begin
     case Sugg of
       {_, Val, _} -> Val;
       _ -> Old
     end
   end || {Old, Sugg} <- lists:zip(Field, Suggestions)].

%% Заполняет случайную клетку допустимым образом (соблюдая правила)
random_step_checked(Field, Parallel) ->
  Suggestions = check_field(Field, Parallel),
  AvailPositions = find_all(false, Suggestions),
  case AvailPositions of
    [] -> {impossibru, Field};
    _ ->
      NextPos = random:uniform(length(AvailPositions)),
      NextVal = random:uniform(2),
      {ok, set_nth(lists:nth(NextPos, AvailPositions), Field, NextVal)}
  end.

%% Очищает клетки, которые можно вывести из соседних
reduce_solvable(Field, Parallel) ->
  Suggestions = check_field(Field, Parallel, true),
  case remove_suggestions(Field, Suggestions) of
    Field -> Field;
    StrippedField ->
%%       print_field(Field),
%%       io:format("Can remove: ~p~n", [Suggestions]),
      reduce_solvable(StrippedField, Parallel)
  end.

%% Применяет к полю указанные предложения
remove_suggestions(Field, Suggestions) ->
  [begin
     case Sugg of
       {_, Old, _} -> 0;
       _ -> Old
     end
   end || {Old, Sugg} <- lists:zip(Field, Suggestions)].

%% Получает для каждой ячейки повод ее заполнить
check_field(Field, Parallel) -> check_field(Field, Parallel, false).
check_field(Field, Parallel, CheckFilled) ->
  Size = field_size(Field),
  apply(case Parallel of true -> fun pmap/2; _ -> fun lists:map/2 end,
    [fun(N) -> check_cell(Field, N, Size, CheckFilled) end, lists:seq(1, Size * Size)]).

%% Проверяет ячейку на все условия, возвращает первый найденный повод заполнить ее, false или filled для уже заполненных
check_cell(Field, N, Size) -> check_cell(Field, N, Size, false).
check_cell(Field, N, Size, CheckFilled) ->
  Funs = [
    {x, fun check_twins/3},
    {x, fun check_pairs/3},
    {x, fun check_others/3},
    {x, fun check_duplicates/3}
  ],
  IsFilled = lists:nth(N, Field) /= 0,
  if
    CheckFilled == IsFilled -> check_until(Funs, [Field, N, Size], fun(Res, _) -> Res end);
    IsFilled -> filled;
    true -> empty
  end.

%% Проверяет, можно ли данную клетку заполнить на основании того, что рядом есть пара одинаковых
check_pairs(Field, N, Size) ->
  Funs = [
    {above, fun get_pair_above/3},
    {right, fun get_pair_right/3},
    {below, fun get_pair_below/3},
    {left, fun get_pair_left/3}],
  check_until(Funs, [Field, N, Size],
    fun
      ([A, A], Tag) when A > 0 -> {pair, 3 - A, Tag};
      (_, _) -> false
    end).

%% Проверяет, можно ли данную клетку заполнить на основании того, она стоит между двумя одинаковыми
check_twins(Field, N, Size) ->
  Funs = [
    {vertical, fun get_twins_vertical/3},
    {horizontal, fun get_twins_horizontal/3}],
  check_until(Funs, [Field, N, Size],
    fun
      ([A, A], Tag) when A > 0 -> {twins, 3 - A, Tag};
      (_, _) -> false
    end).

%% Проверяет, можно ли данную клетку заполнить на основании того, что половина строки или колонки уже заполнена одним цветом
check_others(Field, N, Size) ->
  Funs = [
    {row, fun get_row_others/3},
    {column, fun get_column_others/3}],
  check_until(Funs, [Field, N, Size],
    fun(Res, Tag) ->
      Ones = count_elements(Res, 1),
      Twos = count_elements(Res, 2),
      if
        Ones == Size / 2 -> {others, 2, Tag};
        Twos == Size / 2 -> {others, 1, Tag};
        true -> false
      end
    end).

check_duplicate_row(Field, N, Size) ->
  Row = get_row(Field, N, Size),
  case count_elements(Row, 0) of
    2 ->
      XPos = get_xpos(N, Size),
      case find_duplicate_row(Field, Row, Size) of
        false -> false;
        Res -> {duplicate, 3 - lists:nth(XPos, Res), row}
      end;
    _ -> false
  end.

check_duplicate_column(Field, N, Size) ->
  Column = get_column(Field, N, Size),
  case count_elements(Column, 0) of
    2 ->
      YPos = get_ypos(N, Size),
      case find_duplicate_column(Field, Column, Size) of
        false -> false;
        Res -> {duplicate, 3 - lists:nth(YPos, Res), column}
      end;
    _ -> false
  end.

check_duplicates(Field, N, Size) ->
  case check_duplicate_row(Field, N, Size) of
    false -> check_duplicate_column(Field, N, Size);
    Res -> Res
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Механика игры
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Создает новое пустое поле
new(N) when N > 0, N rem 2 == 0 -> lists:duplicate(N * N, 0).

%% Вычисляет длину стороны поля по массиву-представлению
field_size(Field) -> trunc(math:sqrt(length(Field))).

%% Вспомогательная функция для последовательной проверки условий
check_until([], _, _) -> false;
check_until([{Tag, Fun} | Funs], Args, Handler) ->
  case apply(Handler, [apply(Fun, Args), Tag]) of
    false -> check_until(Funs, Args, Handler);
    X -> X
  end.

get_xpos(N, Size) -> (N - 1) rem Size + 1.
get_ypos(N, Size) -> trunc((N - 1) / Size) + 1.


%% Пары (_**)

get_pair_above(Field, N, Size) ->
  YPos = get_ypos(N, Size),
  if
    YPos =< 2 -> [];
    true -> [lists:nth(N - 2 * Size, Field), lists:nth(N - Size, Field)]
  end.

get_pair_right(Field, N, Size) ->
  XPos = get_xpos(N, Size),
  if
    XPos >= Size - 1 -> [];
    true -> lists:sublist(Field, N + 1, 2)
  end.

get_pair_below(Field, N, Size) ->
  YPos = get_ypos(N, Size),
  if
    YPos >= Size - 1 -> [];
    true -> [lists:nth(N + Size, Field), lists:nth(N + 2 * Size, Field)]
  end.

get_pair_left(Field, N, Size) ->
  XPos = get_xpos(N, Size),
  if
    XPos =< 2 -> [];
    true -> lists:sublist(Field, N - 2, 2)
  end.


%% Двойняшки (*_*)

get_twins_vertical(_, N, Size) when N =< Size; N > Size * (Size - 1) -> []; %% Края
get_twins_vertical(Field, N, Size) -> [lists:nth(N - Size, Field), lists:nth(N + Size, Field)].

get_twins_horizontal(_, N, Size) when N rem Size =< 1 -> []; %% Края
get_twins_horizontal(Field, N, _) ->
  [A, _, B] = lists:sublist(Field, N - 1, 3),
  [A, B].


%% Наполовину заполненные строки и колонки

get_row_others(Field, N, Size) ->
  RowStart = Size * trunc((N - 1) / Size) + 1,
  lists:sublist(Field, RowStart, N - RowStart) ++ lists:sublist(Field, N + 1, RowStart + Size - N - 1).

get_column_others(Field, N, Size) ->
  {_, Res} = lists:foldl(fun get_column_fun/2, {{N rem Size, Size, 1, N}, []}, Field),
  lists:reverse(Res).

get_column_fun(_, {{N, Size, Cur, Skip}, Acc}) when Cur == Skip -> {{N, Size, Cur + 1, Skip}, Acc};
get_column_fun(I, {{N, Size, Cur, Skip}, Acc}) when Cur rem Size == N -> {{N, Size, Cur + 1, Skip}, [I | Acc]};
get_column_fun(_, {{N, Size, Cur, Skip}, Acc}) -> {{N, Size, Cur + 1, Skip}, Acc}.


%% Поиск дублирующихся строк и колонок

get_row(Field, N, Size) ->
  RowStart = Size * trunc((N - 1) / Size) + 1,
  lists:sublist(Field, RowStart, Size).

get_column(Field, N, Size) ->
  {_, Res} = lists:foldl(fun get_column_fun/2, {{N rem Size, Size, 1, -1}, []}, Field),
  lists:reverse(Res).

can_be_same([], []) -> true;
can_be_same([H | T1], [H | T2]) -> can_be_same(T1, T2);
can_be_same([0 | T1], [_ | T2]) -> can_be_same(T1, T2);
can_be_same([_ | T1], [0 | T2]) -> can_be_same(T1, T2);
can_be_same(_, _) -> false.

find_duplicate_row([], _, _) -> false;
find_duplicate_row(Field, SampleRow, Size) ->
  Row = lists:sublist(Field, 1, Size),
  IsFull = 0 == count_elements(Row, 0),
  CanBeSame = can_be_same(SampleRow, Row),
  case IsFull and CanBeSame of
    true -> Row;
    false -> find_duplicate_row(lists:nthtail(Size, Field), SampleRow, Size)
  end.

find_duplicate_column(Field, Sample, Size) ->
  find_duplicate_column(Size, Field, Sample, Size).

find_duplicate_column(0, _, _, _) -> false;
find_duplicate_column(N, Field, Sample, Size) when N > 0 ->
  Column = get_column(Field, N, Size),
  IsFull = 0 == count_elements(Column, 0),
  CanBeSame = can_be_same(Sample, Column),
  case IsFull and CanBeSame of
    true -> Column;
    false -> find_duplicate_column(N - 1, Field, Sample, Size)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Утилиты
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Выводит поле на экран
print_field(Field) when is_list(Field) -> print_field(Field, field_size(Field)), io:format("~n").

print_field([], _) -> ok;
print_field(Field, N) ->
  {Row, Tail} = lists:split(N, Field),
  print_row(Row),
  print_field(Tail, N).

print_row(Row) -> io:format("~s~n", [lists:flatten([[element(X + 1, {$., $O, $X}) | " "] || X <- Row])]).

stats(List) ->
  Len = length(List),
  Mean = lists:sum(List) / Len,
  Variance = math:sqrt(lists:sum([(X - Mean) * (X - Mean) || X <- List]) / Len),
  {mean, trunc(Mean) / 1000, variance, trunc(Variance) / 1000}.

pmap(F, L) ->
  S = self(),
  Ref = erlang:make_ref(),
  Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, Ref, F, I) end) end, L),
  pmap_gather(Pids, Ref).

pmap_f(Parent, Ref, F, I) ->
  Parent ! {self(), Ref, (catch F(I))}.

pmap_gather([Pid | T], Ref) ->
  receive
    {Pid, Ref, Ret} -> [Ret | pmap_gather(T, Ref)]
  end;
pmap_gather([], _) -> [].

set_nth(N, List, Val) when N > 0 -> set_nth([], N, List, Val).
set_nth(Acc, 1, [_ | T], Val) -> lists:reverse(Acc, [Val | T]);
set_nth(Acc, N, [H | T], Val) -> set_nth([H | Acc], N - 1, T, Val).

find_all(Val, List) ->
  {_, RRes} = lists:foldl(fun(I, {N, Acc}) ->
    {N + 1, case I of Val -> [N | Acc]; _ -> Acc end}
  end, {1, []}, List),
  lists:reverse(RRes).

%% Считает в списке количество элементов, равных указанному
count_elements(List, E) ->
  lists:foldl(fun(I, Acc) -> if I == E -> Acc + 1; true -> Acc end end, 0, List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тесты
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  ?assertEqual([0, 0, 0, 0], new(2)),
  ?assertEqual(lists:duplicate(16, 0), new(4)),
  ?assertError(function_clause, new(3)),
  ?assertError(function_clause, new(-1)).

is_solved_test() ->
  ?assert(is_solved([])),
  ?assert(is_solved([1, 2, 2, 1])),
  ?assertNot(is_solved([1, 0, 2, 1])).

get_pair_test() ->
  Input = [
    1, 2, 3,
    4, 5, 6,
    7, 8, 9],
  % {N, above, right, below, left}
  Results = [
    {1, [], [2, 3], [4, 7], []},
    {2, [], [], [5, 8], []},
    {3, [], [], [6, 9], [1, 2]},
    {4, [], [5, 6], [], []},
    {5, [], [], [], []},
    {6, [], [], [], [4, 5]},
    {7, [1, 4], [8, 9], [], []},
    {8, [2, 5], [], [], []},
    {9, [3, 6], [], [], [7, 8]}],
  [begin
     ?assertEqual(R1, get_pair_above(Input, N, 3)),
     ?assertEqual(R2, get_pair_right(Input, N, 3)),
     ?assertEqual(R3, get_pair_below(Input, N, 3)),
     ?assertEqual(R4, get_pair_left(Input, N, 3))
   end || {N, R1, R2, R3, R4} <- Results].

create_column(Column) ->
  Len = length(Column),
  Pad = lists:duplicate(Len - 1, 0),
  lists:flatten([[X | Pad] || X <- Column]).

create_column_test() ->
  ?assertEqual([a, 0, 0, b, 0, 0, c, 0, 0], create_column([a, b, c])).

check_pairs_test() ->
  Field = [
    0, 1, 0, 0, 0, 0,
    0, 1, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0,
    0, 1, 2, 0, 0, 0,
    0, 1, 2, 0, 0, 0,
    0, 1, 1, 0, 0, 0
  ],
  Size = field_size(Field),
  ?assertEqual({pair, 2, above}, check_pairs(Field, 14, Size)),
  ?assertEqual({pair, 1, below}, check_pairs(Field, 15, Size)),
  ?assertEqual({pair, 2, right}, check_pairs(Field, 31, Size)),
  ?assertEqual({pair, 2, left}, check_pairs(Field, 34, Size)),
  ?assertEqual(false, check_pairs(Field, 28, Size)).

check_twins_test() ->
  Field = [
    0, 1, 0, 1, 0, 0,
    0, 1, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0,
    0, 1, 2, 0, 0, 0,
    0, 1, 2, 0, 2, 0,
    0, 1, 1, 0, 0, 0
  ],
  Size = field_size(Field),
  ?assertEqual(false, check_twins(Field, 16, Size)),
  ?assertEqual({twins, 2, horizontal}, check_twins(Field, 3, Size)),
  ?assertEqual({twins, 2, vertical}, check_twins(Field, 14, Size)),
  ?assertEqual({twins, 1, horizontal}, check_twins(Field, 28, Size)),
  ?assertEqual(false, check_twins(Field, 15, Size)).

get_others_test() ->
  Field = [
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, a, b, c,
    d, e, f, g
  ],
  ?assertEqual([5, 6, 7, 8], get_row(Field, 7, 4)),
  ?assertEqual([3, 7, b, f], get_column(Field, 7, 4)),
  ?assertEqual([2, 3, 4], get_row_others(Field, 1, 4)),
  ?assertEqual([5, 7, 8], get_row_others(Field, 6, 4)),
  ?assertEqual([9, a, b], get_row_others(Field, 12, 4)),
  ?assertEqual([5, 9, d], get_column_others(Field, 1, 4)),
  ?assertEqual([2, a, e], get_column_others(Field, 6, 4)),
  ?assertEqual([4, 8, c], get_column_others(Field, 16, 4)).

check_others_test() ->
  Field = [
    0, 0, 0, 1, 0, 0,
    0, 0, 1, 0, 0, 0,
    0, 2, 0, 0, 0, 0,
    0, 1, 2, 0, 0, 0,
    0, 2, 0, 2, 0, 2,
    0, 2, 1, 0, 0, 0
  ],
  Size = field_size(Field),
  ?assertEqual({others, 1, column}, check_others(Field, 2, Size)),
  ?assertEqual({others, 1, column}, check_others(Field, 8, Size)),
  ?assertEqual({others, 1, row}, check_others(Field, 25, Size)),
  ?assertEqual({others, 1, row}, check_others(Field, 27, Size)),
  ?assertEqual(false, check_others(Field, 1, Size)).

can_be_same_test() ->
  ?assert(can_be_same([], [])),
  ?assert(can_be_same([1, 2, 0, 1], [1, 2, 1, 1])),
  ?assert(can_be_same([0, 1, 1, 2], [0, 0, 1, 2])),
  ?assertNot(can_be_same([1, 0, 1, 2], [1, 0, 2, 2])).

check_duplicate_row_test() ->
  Field = [
    2, 1, 1, 2, 2, 1,
    2, 1, 0, 2, 0, 1,
    0, 0, 0, 1, 0, 0,
    1, 0, 0, 1, 0, 0,
    0, 0, 0, 2, 0, 0,
    2, 0, 0, 2, 0, 0
  ],
  Size = field_size(Field),
  ?assertEqual([2, 1, 1, 2, 2, 1], find_duplicate_row(Field, [2, 1, 0, 2, 0, 1], Size)),
  ?assertEqual(false, find_duplicate_row(Field, [1, 1, 0, 2, 0, 1], Size)),
  ?assertEqual([2, 2, 1, 1, 2, 2], find_duplicate_column(Field, [2, 2, 0, 1, 0, 2], Size)),
  ?assertEqual(false, find_duplicate_column(Field, [2, 0, 2, 1, 0, 1], Size)),
  ?assertEqual({duplicate, 2, row}, check_duplicates(Field, 9, Size)),
  ?assertEqual({duplicate, 1, row}, check_duplicates(Field, 11, Size)),
  ?assertEqual({duplicate, 2, column}, check_duplicates(Field, 13, Size)),
  ?assertEqual({duplicate, 1, column}, check_duplicates(Field, 25, Size)),
  ?assertEqual(false, check_duplicates(Field, 21, Size)).

check_cell_test() ->
  Field = [
    0, 1, 0, 1, 0, 0,
    0, 1, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0,
    0, 1, 2, 0, 0, 0,
    0, 1, 2, 0, 0, 0,
    0, 1, 1, 0, 0, 0
  ],
  Size = field_size(Field),
  ?assertEqual(false, check_cell(Field, 1, Size)),
  ?assertEqual({twins, 2, vertical}, check_cell(Field, 14, Size)),
  ?assertEqual({pair, 1, below}, check_cell(Field, 15, Size)),
  ?assertEqual({pair, 2, right}, check_cell(Field, 31, Size)),
  ?assertEqual({pair, 2, left}, check_cell(Field, 34, Size)),
  ?assertEqual({twins, 2, horizontal}, check_cell(Field, 3, Size)),
  ?assertEqual(false, check_cell(Field, 28, Size)),
  ?assertEqual(filled, check_cell(Field, 8, Size)),
  Field2 = [
    1, 0, 0, 1, 0, 0,
    2, 1, 0, 2, 0, 1,
    0, 0, 0, 2, 0, 0,
    0, 0, 0, 1, 0, 0,
    1, 0, 0, 1, 0, 0,
    2, 1, 1, 2, 2, 1
  ],
  Size2 = field_size(Field2),
  ?assertEqual({duplicate, 2, row}, check_cell(Field2, 9, Size2)),
  ?assertEqual({duplicate, 1, row}, check_cell(Field2, 11, Size2)),
  ?assertEqual({duplicate, 1, column}, check_cell(Field2, 13, Size2)),
  ?assertEqual({duplicate, 2, column}, check_cell(Field2, 19, Size2)).

set_nth_test() ->
  ?assertEqual([1, 2, 3, a], set_nth(4, [1, 2, 3, 4], a)),
  ?assertEqual([a, 2, 3, 4], set_nth(1, [1, 2, 3, 4], a)),
  ?assertEqual([1, 2, 3, a, 5, 6, 7], set_nth(4, [1, 2, 3, 4, 5, 6, 7], a)).

find_all_test() ->
  ?assertEqual([], find_all(b, [1, a, 3, 4, a, 6])),
  ?assertEqual([2, 5], find_all(a, [1, a, 3, 4, a, 6])).

get_xypos_test() ->
  ?assertEqual([1, 2, 3, 4, 1, 2, 3, 4], [get_xpos(I, 4) || I <- lists:seq(1, 8)]),
  ?assertEqual([1, 1, 1, 1, 2, 2, 2, 2], [get_ypos(I, 4) || I <- lists:seq(1, 8)]).

apply_suggestions_test() ->
  ?assertEqual([0, 0, 2, 0], apply_suggestions([0, 0, 0, 0], [false, filled, {pair, 2, below}, false])).

-endif.
