-module (vector).

-export ([from_binary/1, partition/2, concat/2, get_size/1, to_binary/1]).
-export_type ([t/0]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type t()    :: binary() | array().
-type pred() :: fun((any()) -> boolean()).

-record (vector, {size    = 0    :: integer(),
                  payload = <<>> :: binary() | array()}).

-spec from_binary(binary()) -> t().
from_binary(<<V/binary>>) ->
  #vector{size    = byte_size(V),
          payload = V}.

-spec to_binary(t()) -> binary().
to_binary(V) -> V#vector.payload.

-spec get_size(t()) -> integer().
get_size(V) -> V#vector.size.

-spec partition(pred(), t()) -> {t(), t()}.
partition(P, #vector{payload = V}) ->
  io:format("~w~n", [V]),
  {Left, Right} = partition_priv(P, V),
  {from_binary(Left), from_binary(Right)}.

-spec partition_priv(pred(), VType) -> {VType, VType} when
  VType :: binary() | array().
partition_priv(P, <<Es/binary>>) ->
  io:format("~w~n", [Es]),
  partition_rec(P, Es, {<<>>, <<>>}).

partition_rec(_, <<>>, Acc) -> Acc;
partition_rec(P, <<E>>, {True, False}) ->
  case P(E) of
    true ->
      {<<True/binary, <<E>>/binary >>, False};
    false ->
      {True, <<False/binary, <<E>>/binary>>}
  end;
partition_rec(P, <<E, Es/binary>>, {True, False}) ->
  case P(E) of
    true ->
      partition_rec(P, Es, {<<True/binary, <<E>>/binary >>, False});
    false ->
      partition_rec(P, Es, {True, <<False/binary, <<E>>/binary>>})
  end.

-spec concat(t(), t()) -> t().
concat(#vector{size = S1, payload = <<V1/binary>>},
       #vector{size = S2, payload = <<V2/binary>>}) ->
  #vector{size = S1 + S2, payload = <<V1/binary, V2/binary>>}.

-ifdef (TEST).

partition_test() ->
  V = from_binary(<<1,2,3,0,0>>),
  P = fun(E) ->
    E == 0
  end,
  ?assertEqual({from_binary(<<0,0>>), from_binary(<<1,2,3>>)},
                partition(P, V)).

-endif.