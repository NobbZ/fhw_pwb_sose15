%% @doc Some tools needed for Erlking
%% @author Norbert Melzer <inf100760@fh-wedel.de>
-module (erlking_tools).

-export ([parse_board/1]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type gameboard() :: matrix:t(byte()).

%% @doc Parse a given string and return a board
-spec parse_board(string()) -> gameboard().
parse_board(Board) ->
  Board_Expr        = string:concat(string:strip(Board, both), "."),
  {ok, Tokens, _}   = erl_scan:string(Board_Expr),
  {ok, [Form]}      = erl_parse:parse_exprs(Tokens),
  {value, Value, _} = erl_eval:expr(Form, []),
  matrix:transpose(Value).

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

-endif.