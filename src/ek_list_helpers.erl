%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2015 17:37
%%%-------------------------------------------------------------------
-module(ek_list_helpers).
-author("Norbert Melzer").

%% API
-export([take/2, drop/2]).

%%% @doc Takes the first `N' elements from `List'.
-spec take(List :: [A], N :: non_neg_integer()) -> [A] when
  A :: term().
take(_, 0)     -> [];
take([], _)    -> [];
take([H|T], N) -> [H|take(T, N - 1)].

%%% @doc Drops the first `N' elements from `List'.
-spec drop(List :: [A], N :: non_neg_integer()) -> [A] when
  A :: term().
drop(L, 0)     -> L;
drop([], _)    -> [];
drop([_|T], N) -> drop(T, N - 1).
