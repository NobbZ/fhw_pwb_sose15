%%%-------------------------------------------------------------------
%%% @author Norbert Melzer <inf100760@fh-wedel.de>
%%% @copyright 2015, Norbert Melzer
%%% @doc Very basic vector functions.
%%%
%%% Currently this is more a fixed-size array of bytes.
%%% @end
%%%-------------------------------------------------------------------
-module(vector).

%%% Constructors
-export([new/1, new/2]).

%%% Convertors
-export([from_binary/1, to_binary/1]).

%%% Accessors
-export([at/2]).

%%% Meta-Data
-export([get_size/1]).

%%% Various
-export([partition/2, concat/2]).

%%% Types
-export_type([t/0, pred/0, payload/0, size/0]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type size() :: non_neg_integer().
%%% A type corresponding to the size of a vector.
-opaque payload() :: binary().
%%% The internal representation of the vector.
-type pred() :: fun((I :: any()) -> boolean()).
%%% A function that returns either `true' or `false' on a given `I', also known
%%% as a predicate.

-record(vector, {size = 0 :: size(),
                 payload = <<>> :: payload()}).

-opaque t() :: #vector{size :: size(), payload :: payload()}.
%%% The type of a vector.

%%% ===========================================================================
%%% Constructors
%%% ===========================================================================

%%% @equiv new(Size, 0)
-spec new(Size :: size()) -> t().
new(Size) ->
  new(Size, 0).

%%% @doc Creates a new vector of size `Size' with all values equal to `Default'.
-spec new(Size :: size(), Default :: byte()) -> t().
new(Size, Default) ->
  Binary = new(Size, Default, <<>>),
  from_binary(Binary).

%%% @hidden
-spec new(Size :: size(), Default :: byte(), Acc :: binary()) -> t().
new(0, _, Acc) -> Acc;
new(Size, Default, Acc) ->
  new(Size - 1, Default, <<Acc/binary, Default/binary>>).

%%% ===========================================================================
%%% Convertors
%%% ===========================================================================

%%% @doc Converts a given vector `V' to a binary.
-spec to_binary(V :: t()) -> binary().
to_binary(V) -> V#vector.payload.

%%% @doc Converts a given binary `B' to a vector of bytes.
-spec from_binary(B :: binary()) -> t().
from_binary(<<B/binary>>) ->
  #vector{size    = byte_size(B),
          payload = B}.

%%% ===========================================================================
%%% Accessors
%%% ===========================================================================

%%% @doc Gets the `N'th element from vector `V'.
%%%
%%% The first element of a vector has the index 0 while the last has Size - 1.
-spec at(V :: t(), N :: size()) -> byte().
at(#vector{size = S, payload = <<B/binary>>}, N) when (N >= 0) and (N < S) ->
  binary:at(B, N - 1).

%%% ===========================================================================
%%% Meta-Data
%%% ===========================================================================

%%% @doc Gets the size of the vector `V'.
-spec get_size(V :: t()) -> size().
get_size(V) -> V#vector.size.

%%% ===========================================================================
%%% Various
%%% ===========================================================================

%%% @doc Filters a vector by `P' and returns both results.
%%%
%%% ```
%%% >>> V = from_binary(<<1,2,3,4,5,6,7,8,9,10>>).
%%% >>> P = fun(X) -> X =< 5 end.
%%% >>> {T, F} = partition(F, V).
%%% >>> to_binary(T).
%%% %=> <<1,2,3,4,5>>
%%% >>> to_binary(F).
%%% %=> <<6,7,8,9,10>>
%%% '''
-spec partition(pred(), t()) -> {t(), t()}.
partition(P, #vector{payload = V}) ->
  {Left, Right} = partition_priv(P, V),
  {from_binary(Left), from_binary(Right)}.

%%% @hidden
-spec partition_priv(pred(), payload()) -> {payload(), payload()}.
partition_priv(P, <<Es/binary>>) ->
  partition_rec(P, Es, {<<>>, <<>>}).

%%% @hidden
-spec partition_rec(pred(), payload(), Acc) -> Result when
  Acc :: {payload(), payload()},
  Result :: {payload(), payload()}.
partition_rec(_, <<>>, Acc) -> Acc;
partition_rec(P, <<E>>, {True, False}) ->
  case P(E) of
    true ->
      {<<True/binary, <<E>>/binary>>, False};
    false ->
      {True, <<False/binary, <<E>>/binary>>}
  end;
partition_rec(P, <<E, Es/binary>>, {True, False}) ->
  case P(E) of
    true ->
      partition_rec(P, Es, {<<True/binary, <<E>>/binary>>, False});
    false ->
      partition_rec(P, Es, {True, <<False/binary, <<E>>/binary>>})
  end.

%%% @doc Concatenates 2 vectors.
-spec concat(t(), t()) -> t().
concat(#vector{size = S1, payload = <<V1/binary>>},
       #vector{size = S2, payload = <<V2/binary>>}) ->
  #vector{size = S1 + S2, payload = <<V1/binary, V2/binary>>}.

-ifdef (TEST).

%%% @hidden
partition_test() ->
  V = from_binary(<<1, 2, 3, 0, 0>>),
  P = fun(E) ->
    E == 0
  end,
  ?assertEqual({from_binary(<<0, 0>>), from_binary(<<1, 2, 3>>)},
               partition(P, V)).

-endif.
