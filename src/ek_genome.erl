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

get_first_normalized([])               -> 0;
get_first_normalized([0|T])            -> get_first_normalized(T);
get_first_normalized([X|_]) when X < 0 -> -1;
get_first_normalized([X|_]) when X > 0 -> 1.

compare(G1, G2, MaxIdx) ->
  F = fun(X, Y) -> X - Y end,
  L1 = ek_list_helpers:take(G1, MaxIdx),
  L2 = ek_list_helpers:take(G2, MaxIdx),
  L3 = lists:zipwith(F, L1, L2),
  get_first_normalized(L3).

equals(G1, G2, MaxIdx) ->
  L1 = ek_list_helpers:take(G1, MaxIdx),
  L2 = ek_list_helpers:take(G2, MaxIdx),
  L1 == L2.

splice(_, G2, 0) -> G2;
splice(G1, G2, SpliceIdx) ->
  G1a = ek_list_helpers:take(G1, SpliceIdx),
  G2a = ek_list_helpers:drop(G2, SpliceIdx),
  G1a ++ G2a.

