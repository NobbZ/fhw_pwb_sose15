%%%-------------------------------------------------------------------
%%% @author nmelzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2015 12:26
%%%-------------------------------------------------------------------
-module(ek_genome).
-author("nmelzer").

%% API
-export([compare/3, equals/3, splice/3]).

take(_, 0)     -> [];
take([], _)    -> [];
take([H|T], N) -> [H|take(T, N - 1)].

drop(L, 0)     -> L;
drop([], _)    -> [];
drop([_|T], N) -> drop(T, N - 1).

get_first_normalized([])               -> 0;
get_first_normalized([0|T])            -> get_first_normalized(T);
get_first_normalized([X|_]) when X < 0 -> -1;
get_first_normalized([X|_]) when X > 0 -> 1.

compare(G1, G2, MaxIdx) ->
  F = fun(X, Y) -> X - Y end,
  L1 = take(G1, MaxIdx),
  L2 = take(G2, MaxIdx),
  L3 = lists:zipwith(F, L1, L2),
  get_first_normalized(L3).

equals(G1, G2, MaxIdx) ->
  L1 = take(G1, MaxIdx),
  L2 = take(G2, MaxIdx),
  L1 == L2.

splice(_, G2, 0) -> G2;
splice(G1, G2, SpliceIdx) ->
  G1a = take(G1, SpliceIdx),
  G2a = drop(G2, SpliceIdx),
  G1a ++ G2a.

