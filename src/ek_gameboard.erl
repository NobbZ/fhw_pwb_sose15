%% @doc This module provides the mechanics for "samegame".
%%
%% @author Norbert Melzer <inf100760@fh-wedel.de>
%% @reference read more about <a href="http://en.wikipedia.org/wiki/SameGame">
%%   samegame</a>.
-module(ek_gameboard).

-export([parse_board/1, grav_board/1, at/2, at/3, flood_find/3,
  find_clickables/1, makemove/2, endgame/1, inc_key/2]).

-export_type([t/0, point/0]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque t() :: matrix:t().
-opaque point() :: {non_neg_integer(), non_neg_integer()}.

%% @doc Parse a given string and return a board.
%%
%% The string has to look like a list of lists, where all sublists have to have
%% the same length and the elements of the sublists need to be integer values.
%% All values will be truncated to 8 bit.
-spec parse_board(string()) -> t().
parse_board(Board) ->
  Board_Expr = string:concat(string:strip(Board, both), "."),
  {ok, Tokens, _} = erl_scan:string(Board_Expr),
  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
  {value, Value, _} = erl_eval:expr(Form, []),
  MatrixRep = matrix:from_list(Value),
  matrix:transpose(MatrixRep).

%% @doc Lets happen gravity on a board.
%%
%% This means that all remaining stones first fall down then all
%% columns which are empty are moved to the right, preserving order of
%% non-empty columns.
-spec grav_board(t()) -> t().
grav_board(Board) ->
  A = down_grav(Board),
  left_grav(A).

%% @doc Access a specific field of the board.
%%
%% `0, 0' is at the lower left corner. Coordinates increase to the right/
%% upwards.
-spec at(t(), non_neg_integer(), non_neg_integer()) -> byte().
at(Board, X, Y) ->
  matrix:at(Board, Y, X).

%% @doc Access a specific field of the board (tuple version).
%%
%% Just maps this call over to `at/3'.
-spec at(t(), {non_neg_integer(), non_neg_integer()}) -> byte().
at(Board, {X, Y}) ->
  at(Board, X, Y).

%% @doc Applies a click and returns the new board.
%%
%% @todo apply grav here and not in `flood_fill/3'.
-spec makemove(t(), {non_neg_integer(), non_neg_integer()}) -> t().
makemove(Board, {X, Y}) ->
  flood_fill(Board, X, Y).

%% @doc Finds one sample-coordinate for every larger group of stones.
-spec find_clickables(t()) -> [{non_neg_integer(), non_neg_integer()}].
find_clickables(Board) ->
  find_clickables(Board,
    0, 0,
    matrix:get_height(Board), matrix:get_width(Board),
    [], sets:new()).

%% @doc Finds one sample-coordinate for every larger group of stones.
-ifdef(OLD_STYLE_TYPES).
-spec find_clickables(Board :: t(),
    X :: non_neg_integer(),
    Y :: non_neg_integer(),
    MaxX :: non_neg_integer(),
    MaxY :: non_neg_integer(),
    Acc :: [{{non_neg_integer(), non_neg_integer()},
      non_neg_integer()}],
    Checked :: set()) ->
  [{{non_neg_integer(), non_neg_integer()},
    non_neg_integer()}].
-else
-spec find_clickables(Board :: t(),
    X :: non_neg_integer(),
    Y :: non_neg_integer(),
    MaxX :: non_neg_integer(),
    MaxY :: non_neg_integer(),
    Acc :: [{{non_neg_integer(), non_neg_integer()},
      non_neg_integer()}],
    Checked :: sets:set(point())) ->
  [{{non_neg_integer(), non_neg_integer()},
    non_neg_integer()}].
-endif.
find_clickables(_Board, _, MY, _, MY, Acc, _) -> Acc;
find_clickables(Board, MX, Y, MX, MY, Acc, Checked) ->
  find_clickables(Board, 0, Y + 1, MX, MY, Acc, Checked);
find_clickables(Board, X, Y, MX, MY, Acc, Checked) when (X < MX) and (Y < MY) ->
  case sets:is_element({X, Y}, Checked) of
    false ->
      FieldsSet = flood_find(Board, X, Y), % flood_find(Board, X, Y),
      NewChecked = sets:union(Checked, FieldsSet),
      SetSize = sets:size(FieldsSet),
      NewAcc = case SetSize >= 2 of
                 true ->
                   [{{X, Y}, SetSize} | Acc];
                 false ->
                   Acc
               end,
      find_clickables(Board, X + 1, Y, MX, MY, NewAcc, NewChecked);
    true ->
      NewAcc = Acc,
      NewChecked = Checked,
      find_clickables(Board, X + 1, Y, MX, MY, NewAcc, NewChecked)
  end.

-spec flood_fill(Board :: t(),
    X :: non_neg_integer(),
    Y :: non_neg_integer()) ->
  t().
flood_fill(Board, X, Y) ->
  Color = at(Board, X, Y),
  if Color /= 0 ->
    FieldsSet = flood_find(Board,
      matrix:get_height(Board),
      matrix:get_width(Board),
      Color,
      [{X, Y}],
      sets:new()),
    StonesAffected = sets:size(FieldsSet),
    Score = (StonesAffected - 1) * (StonesAffected - 1),
    TemporaryBoard = fill(Board, FieldsSet), %, 0, 0),
    NewBoard = grav_board(TemporaryBoard),
    {NewBoard, Score};
    true ->
      {Board, 0}
  end.

-spec fill(Board :: t(),
    Stones :: [{non_neg_integer(), non_neg_integer()}]) ->
  t().
fill(Board, Stones) ->
  F = fun(MX, MY) ->
    case sets:is_element({MX, MY}, Stones) of
      true -> 0;
      false -> ek_gameboard:at(Board, MX, MY)
    end
  end,
  matrix:transpose(matrix:map_pos(F, Board)).

-spec flood_find(t(), non_neg_integer(), non_neg_integer()) -> set().
flood_find(Board, X, Y) ->
  Color = at(Board, X, Y),
  if
    Color /= 0 ->
      flood_find(Board, matrix:get_height(Board), matrix:get_width(Board), Color, [{X, Y}], sets:new());
    true ->
      sets:new()
  end.

flood_find(_, _, _, _, [], Acc) -> Acc;
flood_find(Board, MX, MY, Color, [{X, Y} | Stack], Acc) when (X >= 0) and (Y >= 0) and (X < MX) and (Y < MY) ->
  CurColor = at(Board, X, Y),
  Checked = sets:is_element({X, Y}, Acc),
  {NewAcc, NewStack} = if (CurColor == Color) and not Checked ->
    {sets:add_element({X, Y}, Acc),
      [{X + 1, Y},
        {X - 1, Y},
        {X, Y + 1},
        {X, Y - 1} | Stack]};
                         true ->
                           {Acc, Stack}
                       end,
  flood_find(Board, MX, MY, Color, NewStack, NewAcc);
flood_find(Board, MX, MY, Color, [{_, _} | Stack], Acc) ->
  flood_find(Board, MX, MY, Color, Stack, Acc).

down_grav(Board) ->
  Columns = matrix:to_row_vecs(Board),
  NewColumns = lists:map(fun(Vec) ->
    {Front, Back} = vector:partition(fun(F) -> F /= 0 end, Vec),
    vector:concat(Front, Back)
  end, Columns),
  matrix:from_row_vecs(NewColumns).

-spec endgame(t()) -> integer().
endgame(B) ->
  V0 = matrix:to_row_vecs(B),
  V1 = lists:foldl(fun(V, Acc) -> vector:concat(Acc, V) end, vector:from_binary(<<>>), V0),
  List = binary_to_list(vector:to_binary(V1)),
  Stones = lists:foldl(fun(E, Store) -> inc_key(E, Store) end, [], List),
  Stones1 = lists:keydelete(0, 1, Stones),
  lists:foldr(fun({_, V}, Acc) -> Acc - (V - 1) * (V - 1) end, 0, Stones1).

inc_key(Key, Store) ->
  Val = case lists:keysearch(Key, 1, Store) of
          {value, {_, V}} -> V;
          false -> 0
        end,
  lists:keystore(Key, 1, Store, {Key, Val + 1}).

% lists:map(fun(L) ->
%   {Front, Back} = lists:partition(fun(Field) -> Field /= 0 end, L),
%   Front ++ Back
% end, Board).

left_grav(Board) ->
  Rows = matrix:to_row_vecs(Board),
  {Front, Back} = lists:partition(fun(Col) ->
    case vector:to_binary(Col) of
      <<0, _/binary>> ->
        false;
      _ ->
        true
    end
  end, Rows),
  matrix:from_row_vecs(Front ++ Back).

% {Front, Back} = lists:partition(fun(Col) -> lists:nth(1, Col) /= 0 end, Board),
% Front ++ Back.

% ==============================================================================
% Tests
% ==============================================================================

-ifdef(PERF).

horse_parse() ->
  horse:repeat(1000000, ok).

-endif.

-ifdef(TEST).

example_board() ->
  parse_board("[[0,2,2,0,3],[0,1,2,3,4],[0,0,1,2,3],[0,0,2,2,3],[0,0,0,0,0]]").

parse_board_test() ->
  ?assertEqual(parse_board("[[1,2,3]]"), matrix:from_list([[1], [2], [3]])),
  ?assertEqual(parse_board("[[1,2,3],[4,5,6],[7,8,9]]"), matrix:from_list([[1, 4, 7], [2, 5, 8], [3, 6, 9]])).

parse_board_with_whitespace_test() ->
  ?assertEqual(parse_board("[[1, 2, 3]]"), matrix:from_list([[1], [2], [3]])),
  ?assertEqual(parse_board("[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"), matrix:from_list([[1, 4, 7], [2, 5, 8], [3, 6, 9]])).

down_grav_test() ->
  B = example_board(),
  ?assertEqual(down_grav(B), matrix:from_list([[0, 0, 0, 0, 0],
    [2, 1, 0, 0, 0],
    [2, 2, 1, 2, 0],
    [3, 2, 2, 0, 0],
    [3, 4, 3, 3, 0]])).

left_grav_test() ->
  B = example_board(),
  B2 = down_grav(B),
  ?assertEqual(left_grav(B2), matrix:from_list([[2, 1, 0, 0, 0],
    [2, 2, 1, 2, 0],
    [3, 2, 2, 0, 0],
    [3, 4, 3, 3, 0],
    [0, 0, 0, 0, 0]])).

grav_board_test() ->
  B = example_board(),
  ?assertEqual(grav_board(B), matrix:from_list([[2, 1, 0, 0, 0],
    [2, 2, 1, 2, 0],
    [3, 2, 2, 0, 0],
    [3, 4, 3, 3, 0],
    [0, 0, 0, 0, 0]])).

makemove_test() ->
  B = example_board(),
  {New, Score} = makemove(B, {1, 0}),
  ?assertEqual(New, matrix:from_list([[1, 0, 0, 0, 0],
    [1, 2, 0, 0, 0],
    [3, 2, 2, 0, 0],
    [3, 4, 3, 3, 0],
    [0, 0, 0, 0, 0]])),
  ?assertEqual(Score, 4).

makemove_bug_test() ->
  B = parse_board("[[1,2,3],[1,2,3],[1,2,3]]"),
  ?assertEqual({matrix, 3, 3, <<1, 1, 1, 2, 2, 2, 3, 3, 3>>},
    B),
  {B2, _} = makemove(B, {2, 0}),
  ?assertEqual({matrix, 3, 3, <<1, 1, 1, 2, 2, 2, 0, 0, 0>>},
    B2).

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
  Set = flood_find(B, 1, 0),
  List = sets:to_list(Set),
  ?assertEqual(lists:sort(List), [{1, 0}, {2, 0}, {2, 1}]).

find_clickables_test() ->
  B = example_board(),
  List = find_clickables(B),
  ?assertEqual([{{4, 2}, 2}, {{3, 2}, 3}, {{1, 0}, 3}], List).

find_clickables_bug_test() ->
  B = parse_board("[[1,2,0],[1,2,0],[1,2,0]]"),
  List = find_clickables(B),
  ?assertEqual([{{1, 0}, 3},
    {{0, 0}, 3}],
    List).

find_clickables_moved_test() ->
  B = example_board(),
  C = grav_board(B),
  List = find_clickables(C),
  ?assertEqual([{{0, 0}, 5}, {{2, 0}, 2}, {{3, 2}, 2}], lists:sort(List)).

rectangular_parse_test() ->
  B = parse_board("[[1,2,3],[1,2,3]]"),
  ?assertEqual({matrix, 2, 3, <<1, 1, 2, 2, 3, 3>>}, B).

rectangular_at_test() ->
  B = parse_board("[[1,2,3],[1,2,3]]"),
  ?assertEqual(1, at(B, 0, 0)),
  ?assertEqual(2, at(B, 1, 0)),
  ?assertEqual(3, at(B, 2, 1)).

rectangular_flood_fill_test() ->
  B = parse_board("[[1,2,3],[1,2,3]]"),
  {B1, _} = flood_fill(B, 0, 0),
  Exp = parse_board("[[2,3,0],[2,3,0]]"),
  ?assertEqual(Exp, B1).

-endif.
