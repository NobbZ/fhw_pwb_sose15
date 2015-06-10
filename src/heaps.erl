-module (heaps).
-export ([new/0, new/1, add/2, drop/1, from_list/1, from_list/2, is_empty/1,
          fetch/1, peek/1]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record (prioq, {heap = nil, size = 0, compare = fun def_compare/2}).

def_compare(E1, E2) ->
  if E1 <  E2 -> lt;
     E1 == E2 -> eq;
     E1 >  E2 -> gt
  end.

%% @doc Create an empty priorityqueue.
new() -> #prioq{}.

%% @doc Create an empty priorityqueue with a given `Compare'-function.
%% 
%% The `Compare'-function needs to return `lt' when `E1' is lesser than `E2`,
%% `eq' on equality, and `gt' otherwise.
new(Compare) -> #prioq{compare = Compare}.

%% @doc Adds an element `E' to the priority queue `Q'.
add(_Q = #prioq{heap = Heap, size = N, compare = Compare}, E) ->
  #prioq{heap = meld(Heap, {E, []}, Compare), size = N+1}.

drop(_Q = #prioq{heap = {_, Sub}, size = N, compare = Compare}) ->
  #prioq{heap = pair(Sub, Compare), size = N-1}.

from_list(L) ->
  lists:foldl(fun(E, Q) -> add(Q, E) end, new(), L).

from_list(L, Compare) ->
  lists:foldl(fun(E, Q) -> add(Q, E) end, new(Compare), L).

to_list(#prioq{heap = nil, size = 0}) -> [];
to_list(Q) ->
  [peek(Q) | to_list(drop(Q))].

is_empty(#prioq{heap = nil, size = 0}) -> true;
is_empty(_) -> false.

fetch(Q) ->
  {peek(Q), drop(Q)}.

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