%% @doc Some tools needed for Erlking
%% @author Norbert Melzer <inf100760@fh-wedel.de>
-module (erlking_tools).

-export ([parse_board/1, grav_board/1]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type gameboard() :: matrix:t(byte()).

%% @doc Parse a given string and return a board.
%% Be aware of the fact, that the in memory representation of the board is a
%% transposed version of the original one, so `x' and `y' are swapped!
-spec parse_board(string()) -> gameboard().
parse_board(Board) ->
  Board_Expr        = string:concat(string:strip(Board, both), "."),
  {ok, Tokens, _}   = erl_scan:string(Board_Expr),
  {ok, [Form]}      = erl_parse:parse_exprs(Tokens),
  {value, Value, _} = erl_eval:expr(Form, []),
  matrix:transpose(Value).

%% @doc Lets happen gravity on a board.
-spec grav_board(gameboard()) -> gameboard().
grav_board(Board) ->
  A = down_grav(Board),
  left_grav(A).

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

parse_board_test() ->
  ?assertEqual(parse_board("[[1,2,3]]"), [[1],[2],[3]]),
  ?assertEqual(parse_board("[[1,2,3],[4,5,6],[7,8,9]]"), [[1,4,7],[2,5,8],[3,6,9]]).

parse_board_with_whitespace_test() ->
  ?assertEqual(parse_board("[[1, 2, 3]]"), [[1],[2],[3]]),
  ?assertEqual(parse_board("[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"), [[1,4,7],[2,5,8],[3,6,9]]).

down_grav_test() ->
  B = parse_board("[[0,2,2,0,3],[0,1,2,3,4],[0,0,1,2,3],[0,0,2,2,3],[0,0,0,0,0]]"),
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
  B = parse_board("[[0,2,2,0,3],[0,1,2,3,4],[0,0,1,2,3],[0,0,2,2,3],[0,0,0,0,0]]"),
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
  B = parse_board("[[0,2,2,0,3],[0,1,2,3,4],[0,0,1,2,3],[0,0,2,2,3],[0,0,0,0,0]]"),
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

-endif.