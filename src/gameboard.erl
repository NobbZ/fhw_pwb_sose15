-module (gameboard).

-type t() :: matrix:t(byte()).

-export ([parse_board/1, grav_board/1, at/2, at/3, flood_find/3,
          find_clickables/1, makemove/2]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Parse a given string and return a board.
%% Be aware of the fact, that the in memory representation of the board is a
%% transposed version of the original one, so `x' and `y' are swapped!
-spec parse_board(string()) -> t().
parse_board(Board) ->
  Board_Expr        = string:concat(string:strip(Board, both), "."),
  {ok, Tokens, _}   = erl_scan:string(Board_Expr),
  {ok, [Form]}      = erl_parse:parse_exprs(Tokens),
  {value, Value, _} = erl_eval:expr(Form, []),
  matrix:transpose(Value).

%% @doc Lets happen gravity on a board.
-spec grav_board(t()) -> t().
grav_board(Board) ->
  A = down_grav(Board),
  left_grav(A).

%% @doc Access a specific field of the board.
-spec at(t(), non_neg_integer(), non_neg_integer()) -> byte().
at(Board, X, Y) ->
  matrix:at(Board, Y, X).

%% @doc Access a specific field of the board (tuple version).
-spec at(t(), {non_neg_integer(), non_neg_integer()}) -> byte().
at(Board, {X, Y}) ->
  at(Board, X, Y).

makemove(Board, {X, Y}) ->
  flood_fill(Board, X, Y).

-spec find_clickables(t()) -> [{non_neg_integer(), non_neg_integer()}].
find_clickables(Board) ->
  find_clickables(Board, Board, 0, 0, [], sets:new()).

%% @TODO Really ugly...
find_clickables(_Board, [], _, _, Acc, _) -> Acc;
find_clickables(Board, [[]|Fss], X, _, Acc, Checked) ->
  find_clickables(Board, Fss, X+1, 0, Acc, Checked);
find_clickables(Board, [[_|Fs]|Fss], X, Y, Acc, Checked) ->
  IsElement = sets:is_element({X, Y}, Checked),
  if
    IsElement ->
      find_clickables(Board, [Fs|Fss], X, Y+1, Acc, Checked);
    true -> % This means else!
      FieldsSet = flood_find(Board, X, Y),
      %FieldsSet = sets:from_list(Fields),
      NewChecked = sets:union(Checked, FieldsSet),
      SetSize = sets:size(FieldsSet),
      if
        SetSize > 1 ->
          NewAcc = [{X, Y}|Acc];
        true -> % Really, this IS else!
          NewAcc = Acc
      end,
      find_clickables(Board, [Fs|Fss], X, Y+1, NewAcc, NewChecked)
  end.

flood_fill(Board, X, Y) ->
  Color = at(Board, X, Y),
  if Color /= 0 ->
    FieldsSet = flood_find(Board, matrix:num_cols(Board), matrix:num_rows(Board), Color, [{X, Y}], sets:new()),
    StonesAffected = sets:size(FieldsSet),
    Score = (StonesAffected - 1) * (StonesAffected - 1),
    TemporaryBoard = fill(Board, FieldsSet, 0, 0),
    NewBoard = grav_board(TemporaryBoard);
  true ->
    Score = 0,
    NewBoard = Board
  end,
  {NewBoard, Score}.

fill([], _, _, _) -> [];
fill([Fs|Fss], Stones, X, Y) ->
  [fill_line(Fs, Stones, X, Y) | fill(Fss, Stones, X+1, Y)].

fill_line([], _, _, _) -> [];
fill_line([F|Fs], Stones, X, Y) ->
  IsElement = sets:is_element({X, Y}, Stones),
  NewColor  = if IsElement -> 0;
                 true      -> F
  end,
  [NewColor | fill_line(Fs, Stones, X, Y+1)].

flood_find(Board, X, Y) ->
  Color =  at(Board, X, Y),
  if
    Color /= 0 ->
      flood_find(Board, matrix:num_cols(Board), matrix:num_rows(Board), Color, [{X, Y}], sets:new());
    true ->
      sets:new()
  end.

flood_find(_,     _,  _,  _,     [],             Acc) -> Acc;
flood_find(Board, MX, MY, Color, [{X, Y}|Stack], Acc) when (X>=0) and (Y>=0) and (X<MX) and (Y<MY) ->
  CurColor = at(Board, X, Y),
  Checked  = sets:is_element({X,Y}, Acc),
  if (CurColor == Color) and not Checked ->
      NewAcc   = sets:add_element({X,Y}, Acc),
      NewStack = [{X+1,Y}, {X-1,Y}, {X,Y+1}, {X,Y-1}|Stack];
    true ->
      NewAcc   = Acc,
      NewStack = Stack
  end,
  flood_find(Board, MX, MY, Color, NewStack, NewAcc);
flood_find(Board, MX, MY, Color, [{_, _}|Stack], Acc) ->
  flood_find(Board, MX, MY, Color, Stack, Acc).

down_grav(Board) ->
  lists:map(fun(L) ->
    {Front, Back} = lists:partition(fun(Field) -> Field /= 0 end, L),
    Front ++ Back
  end, Board).

left_grav(Board) ->
  {Front, Back} = lists:partition(fun(Col) -> lists:nth(1, Col) /= 0 end, Board),
  Front ++ Back.

% ==============================================================================
% Tests
% ==============================================================================

-ifdef(TEST).

example_board() ->
  parse_board("[[0,2,2,0,3],[0,1,2,3,4],[0,0,1,2,3],[0,0,2,2,3],[0,0,0,0,0]]").

parse_board_test() ->
  ?assertEqual(parse_board("[[1,2,3]]"), [[1],[2],[3]]),
  ?assertEqual(parse_board("[[1,2,3],[4,5,6],[7,8,9]]"), [[1,4,7],[2,5,8],[3,6,9]]).

parse_board_with_whitespace_test() ->
  ?assertEqual(parse_board("[[1, 2, 3]]"), [[1],[2],[3]]),
  ?assertEqual(parse_board("[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"), [[1,4,7],[2,5,8],[3,6,9]]).

down_grav_test() ->
  B = example_board(),
  ?assertEqual(B, [[0,0,0,0,0],
                   [2,1,0,0,0],
                   [2,2,1,2,0],
                   [0,3,2,2,0],
                   [3,4,3,3,0]]),
  ?assertEqual(down_grav(B), [[0,0,0,0,0],
                              [2,1,0,0,0],
                              [2,2,1,2,0],
                              [3,2,2,0,0],
                              [3,4,3,3,0]]).

left_grav_test() ->
  B = example_board(),
  ?assertEqual(B, [[0,0,0,0,0],
                   [2,1,0,0,0],
                   [2,2,1,2,0],
                   [0,3,2,2,0],
                   [3,4,3,3,0]]),
  B2 = down_grav(B),
  ?assertEqual(left_grav(B2), [[2,1,0,0,0],
                               [2,2,1,2,0],
                               [3,2,2,0,0],
                               [3,4,3,3,0],
                               [0,0,0,0,0]]).

grav_board_test() ->
  B = example_board(),
  ?assertEqual(B, [[0,0,0,0,0],
                   [2,1,0,0,0],
                   [2,2,1,2,0],
                   [0,3,2,2,0],
                   [3,4,3,3,0]]),
  ?assertEqual(grav_board(B), [[2,1,0,0,0],
                                [2,2,1,2,0],
                                [3,2,2,0,0],
                                [3,4,3,3,0],
                                [0,0,0,0,0]]).

makemove_test() ->
  B = example_board(),
  {New, Score} = makemove(B, {1,0}),
  ?assertEqual(New, [[1,0,0,0,0],
                     [1,2,0,0,0],
                     [3,2,2,0,0],
                     [3,4,3,3,0],
                     [0,0,0,0,0]]),
  ?assertEqual(Score, 4).

at_non_tuple_test() ->
  B = example_board(),
  ?assertEqual(at(B, 0, 0), 0),
  ?assertEqual(at(B, 1, 0), 2),
  ?assertEqual(at(B, 4, 3), 3).

at_tuple_test() ->
  B = example_board(),
  ?assertEqual(at(B, {0, 0}), 0),
  ?assertEqual(at(B, {1, 0}), 2),
  ?assertEqual(at(B, {4, 3}), 3).

flood_find_test() ->
  B = example_board(),
  Set  = flood_find(B, 1, 0),
  List = sets:to_list(Set),
  ?assertEqual(lists:sort(List), [{1,0},{2,0},{2,1}]).

find_clickables_test() ->
  B = example_board(),
  List = find_clickables(B),
  ?assertEqual(lists:sort(List), [{1,0},{2,3},{4,2}]).

find_clickables_moved_test() ->
  B = example_board(),
  C = grav_board(B),
  List = find_clickables(C),
  ?assertEqual(lists:sort(List), [{0,0},{2,0},{3,2}]).

-endif.