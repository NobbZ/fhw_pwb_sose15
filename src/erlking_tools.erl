%% @doc Some tools needed for Erlking
%% @author Norbert Melzer <inf100760@fh-wedel.de>
-module (erlking_tools).

-export ([parse_board/1, transpose/1]).

parse_board(Board) ->
  Board_Expr        = string:concat(string:strip(Board, both), "."),
  {ok, Tokens, _}   = erl_scan:string(Board_Expr),
  {ok, [Form]}      = erl_parse:parse_exprs(Tokens),
  {value, Value, _} = erl_eval:expr(Form, []),
  transpose(Value).

transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].