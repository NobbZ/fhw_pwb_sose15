-module (heaps).
-export ([new/0, new/1, add/2, drop/1, from_list/1, from_list/2, is_empty/1,
          fetch/1, peek/1, to_list/1]).

-export_type([compare/0, t/0]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type compare() :: fun((any(), any()) -> lt | eq | gt).

-record (prioq, {heap = nil :: nil | tuple(),
                 size = 0   :: integer(),
                 compare = fun def_compare/2 :: compare()
                }
        ).

-opaque t() :: #prioq{}.

-spec def_compare(any(), any()) -> lt | eq | gt.
def_compare(E1, E2) ->
  if E1 <  E2 -> lt;
     E1 == E2 -> eq;
     E1 >  E2 -> gt
  end.

%% @doc Create an empty priorityqueue.
-spec new() -> t().
new() -> #prioq{}.

%% @doc Create an empty priorityqueue with a given `Compare'-function.
%% 
%% The `Compare'-function needs to return `lt' when `E1' is lesser than `E2`,
%% `eq' on equality, and `gt' otherwise.
-spec new(compare()) -> t().
new(Compare) -> #prioq{compare = Compare}.

%% @doc Adds an element `E' to the priority queue `Q'.
-spec add(t(), any()) -> t().
add(_Q = #prioq{heap = Heap, size = N, compare = Compare}, E) ->
  #prioq{heap = meld(Heap, {E, []}, Compare), size = N+1}.

-spec drop(t()) -> t().
drop(_Q = #prioq{heap = {_, Sub}, size = N, compare = Compare}) ->
  #prioq{heap = pair(Sub, Compare), size = N-1}.

-spec from_list(list()) -> t().
from_list(L) ->
  lists:foldl(fun(E, Q) -> add(Q, E) end, new(), L).

-spec from_list(list(), compare()) -> t().
from_list(L, Compare) ->
  lists:foldl(fun(E, Q) -> add(Q, E) end, new(Compare), L).

-spec to_list(t()) -> list().
to_list(#prioq{heap = nil, size = 0}) -> [];
to_list(Q) ->
  [peek(Q) | to_list(drop(Q))].

-spec is_empty(t()) -> boolean().
is_empty(#prioq{heap = nil, size = 0}) -> true;
is_empty(_) -> false.

-spec fetch(t()) -> {any(), t()}.
fetch(Q) ->
  {peek(Q), drop(Q)}.

-spec peek(t()) -> any().
peek(_Q = #prioq{heap = {X, _}}) ->
  X.






meld(nil, Q, _Compare) -> Q;
meld(Q, nil, _Compare) -> Q;
meld(Left = {X, SubLeft}, Right = {Y, SubRight}, Compare) ->
  case Compare(X, Y) of
    lt ->
      {X, [Right|SubLeft ]};
    _ ->
      {Y, [Left |SubRight]}
  end.

pair([],  _Compare) -> nil;
pair([Q], _Compare) -> Q;
pair([Q0, Q1 | Qs], Compare) ->
  Q2 = meld(Q0, Q1, Compare),
  meld(Q2, pair(Qs, Compare), Compare).

-ifdef (TEST).

example_queue() ->
  Q0 = new(),
  Q1 = add(Q0, 4),
  Q2 = add(Q1, 3),
  Q3 = add(Q2, 10),
  add(Q3, 1).

new_def_compare_test() ->
  ?assertMatch(#prioq{heap = nil, size = 0}, new()).

add_smaller_test() ->
  E1 = peek(example_queue()),
  E0 = E1 - 1,
  Q = add(example_queue(), E0),
  ?assertEqual(peek(Q), E0).

add_greater_test() ->
  E1 = peek(example_queue()),
  E0 = E1 + 1,
  Q = add(example_queue(), E0),
  ?assertEqual(peek(Q), E1).

drop_test() ->
  Q0 = example_queue(),
  Q1 = drop(Q0),
  ?assertMatch(#prioq{heap = {3,[{10,[]},{4,[]}]}, size = 3}, Q1).

from_and_to_list_test() ->
  L = [1,2,3,4,5],
  Q = from_list(L),
  ?assertEqual(to_list(Q), L).

is_not_empty_test() ->
  ?assertNot(is_empty(example_queue())).

is_empty_test() ->
  ?assert(is_empty(new())).
  
-endif.
