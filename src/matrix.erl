%% @doc
%% @author Norbert Melzer <inf100760@fh-wedel.de>
%% @reference See <a href="http://en.wikipedia.org/wiki/Matrix_(mathematics)">
%% Wikipedia article about matrixes</a> for further information.
-module (matrix).

-export ([transpose/1, from_list/1, matrix/3, at/3, to_row_vecs/1, from_row_vecs/1]).
-export_type ([t/0]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record (matrix, {width   = 0    :: non_neg_integer(),
                  height  = 0    :: non_neg_integer(),
                  payload = <<>> :: array() | binary()}).

-opaque t()       :: #matrix{width :: non_neg_integer(), height :: non_neg_integer(), payload :: array() | binary() }.
-type generator() :: fun((non_neg_integer(), non_neg_integer()) -> any()).

-define (inbetween (V, Min, Max), (((Min) =< (V)) and ((V) =< (Max)))).
-define (is_byte (V), ?inbetween(V, 16#00, 16#ff)).
-define (else, true).

% ==============================================================================
% Retrieve information about the matrix
% ==============================================================================

%% @doc Retrieves the number of rows in a given matrix.
-spec get_height(t()) -> pos_integer().
get_height(#matrix{height = H}) -> H.

%% @doc Retrieves the mumber of columns in a given matrix.
-spec get_width(t()) -> pos_integer().
get_width(#matrix{width = W}) -> W.

% ==============================================================================
% Builders
% ==============================================================================

-spec new(non_neg_integer(), non_neg_integer(), any()) -> t().
new(Width, Height, Default) ->
  List    = lists:duplicate(Width * Height, Default),
  Payload = if is_integer(Default) and ?inbetween(Default, 16#00, 16#ff) ->
    list_to_binary(List);
  ?else ->
    array:fix(array:from_list(List))
  end,
  #matrix{width = Width, height = Height, payload = Payload}.

-spec from_list([[any()]]) -> t().
from_list(List) ->
  Width = lists:max(lists:map(fun(E) ->
    lists:foldl(fun(_, Len) ->
      1 + Len
    end, 0, E)
  end, List)),
  Height = lists:foldl(fun(_, Len) -> 1 + Len end, 0, List),
  FlatList = lists:concat(List),
  AllInts = lists:all(fun is_integer/1, FlatList),
  BinaryFits = if AllInts ->
    lists:all(fun(X) -> ?is_byte(X) end, FlatList);
  ?else ->
    false
  end,
  Payload = if BinaryFits ->
    list_to_binary(FlatList);
  ?else ->
    array:fix(array:from_list(FlatList))
  end,
  #matrix{width = Width, height = Height, payload = Payload}.

-spec from_row_vecs([vector:t()]) -> t().
from_row_vecs(Vs) when is_list(Vs) ->
  from_row_vecs(Vs, #matrix{}).

-spec from_row_vecs([vector:t()], t()) -> t().
from_row_vecs([], M) -> M;
from_row_vecs([V|Vs], #matrix{width = W, height = H, payload = <<M/binary>>}) ->
  NewH = H + 1,
  NewW = vector:get_size(V),
  VBin = vector:to_binary(V),
  NewM = <<M/binary, VBin/binary>>,
  from_row_vecs(Vs, #matrix{width = NewW, height = NewH, payload = NewM}).

%% @doc Generate a matrix from a generator function.
-spec matrix(pos_integer(), pos_integer(), generator()) -> t().
matrix(Width, Height, GenFun) ->
  B = lists:sort(lists:flatten(lists:duplicate(Width, lists:seq(0, Height - 1)))),
  A = lists:flatten(lists:duplicate(Height, lists:seq(0, Width - 1))),
  C = lists:zipwith(GenFun, A, B),
  AllInts = lists:all(fun is_integer/1, C),
  BinaryFits = if AllInts ->
    lists:all(fun(X) -> ?is_byte(X) end, C);
  ?else ->
    false
  end,
  Payload = if BinaryFits ->
    list_to_binary(C);
  ?else ->
    array:fix(array:from_list(C))
  end,
  #matrix{width = Width, height = Height, payload = Payload}.

  %splice_list(C, Cols).

-spec splice_list([any()], pos_integer()) -> t().
splice_list(List, Cols) -> lists:reverse(splice_list(List, Cols, [])).

-spec splice_list([any()], pos_integer, [[any()]]) -> [[any()]].
splice_list([], _Cols, Acc) -> Acc;
splice_list(List, Cols, Acc) ->
  {Vec, Rest} = lists:split(Cols, List),
  splice_list(Rest, Cols, [Vec|Acc]).

% ==============================================================================
% Simple transformations
% ==============================================================================

%% @doc Transposes a matrix.
%% Transposing a matrix means to reflect the matrix <math><mtext>M</mtext></math>
%% over its main diagonal to obtain <math><msup><mtext>M</mtext><mtext>T</mtext></msup></math>.
%%
%% == Examples ==
%%
%% ```
%% >>> M = [[1,2],[3,4]].
%% [[1,2],[3,4]]
%% >>> transpose(M).
%% [[1,3],[2,4]]
%% >>> transpose(transpose(M))
%% [[1,2],[3,4]]
%% '''
-spec transpose(t()) -> t().
transpose(#matrix{width = W, height = H} = FullM) ->
  matrix(H, W, fun(X, Y) ->
    at(FullM, Y, X)
  end).

% transpose([[]|_]) -> [];
% transpose(M) ->
%   [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

-spec map_pos(generator(), t()) -> t().
map_pos(GenFun, #matrix{width = W, height = H}) ->
  matrix(H, W, GenFun).

% ==============================================================================
% Retrieve subvectors
% ==============================================================================

-spec to_row_vecs(t()) -> [vector:t()].
to_row_vecs(#matrix{width = W, height = H, payload = <<M/binary>>}) ->
  RowIdxs = lists:seq(0, H - 1),
  lists:map(fun(Row) ->
    vector:from_binary(binary_part(M, W * Row, W))
  end, RowIdxs).

% @doc Gets the specified line-vector.
% Counting starts with 0.
% -spec get_line_vector(t(), non_neg_integer()) -> vector:t().
% get_line_vector(Matrix, Line) -> 
%   lists:nth(Line + 1, Matrix).

% %% @doc Gets the specified column-vector.
% %% Counting starts with 0.
% -spec get_column_vector(t(), non_neg_integer()) -> vector:t().
% get_column_vector(Matrix, Column) ->
%   get_line_vector(transpose(Matrix), Column).

% ==============================================================================
% Retrieve single elements
% ==============================================================================

-spec at(t(), non_neg_integer(), non_neg_integer()) -> any().
at(#matrix{width = W, height = H, payload = <<M/binary>>}, X, Y) when ?inbetween(X, 0, (W - 1)) and ?inbetween(Y, 0, (H - 1)) ->
  binary:at(M, Y * W + X);
at(#matrix{width = W, height = H, payload = M}, X, Y) when ?inbetween(X, 0, (W - 1)) and ?inbetween(Y, 0, (H - 1)) ->
  array:get(Y * W + X, M).
% at(M, X, Y) ->
%   V = lists:nth(Y + 1, M),
%   lists:nth(X + 1, V).

% ==============================================================================
% Tests
% ==============================================================================

-ifdef (TEST).

get_width_test() ->
  M1 = from_list([[1,2,3],[4,5,6],[7,8,9]]),
  M2 = from_list([[1,2,3]]),
  ?assertEqual(3, get_width(M1)),
  ?assertEqual(3, get_width(M2)).

get_height_test() ->
  M1 = from_list([[1,2,3],[4,5,6],[7,8,9]]),
  M2 = from_list([[1,2,3]]),
  ?assertEqual(3, get_height(M1)),
  ?assertEqual(1, get_height(M2)).

from_list_test() ->
  List = [[1,2,3],[4,5,6],[7,8,9]],
  ?assertEqual(#matrix{width = 3, height = 3, payload = <<1,2,3,4,5,6,7,8,9>>},
               from_list(List)).

matrix_test() ->
  ?assertEqual(#matrix{width = 3, height = 3, payload = <<1,2,3,2,4,6,3,6,9>>},
               matrix(3, 3, fun(X,Y) -> (X+1) * (Y+1) end)).

transpose_1_x_m_test() ->
  MOrig = from_list([[1,2,3]]),
  MExp  = from_list([[1],[2],[3]]),
  ?assertEqual(MExp, transpose(MOrig)).
%  ?assertEqual(transpose([[1,2,3]]), [[1],[2],[3]]).

transpose_n_x_1_test() ->
  MOrig = from_list([[1],[2],[3]]),
  MExp  = from_list([[1,2,3]]),
  ?assertEqual(MExp, transpose(MOrig)).

transpose_n_x_m_test() ->
  MOrig = from_list([[1,2,3],[4,5,6],[7,8,9]]),
  MExp  = from_list([[1,4,7],[2,5,8],[3,6,9]]),
  ?assertEqual(MExp, transpose(MOrig)).

transpose_transpose_is_id_test() ->
  M = from_list([[1,4,7],[2,5,8],[3,6,9]]),
  ?assertEqual(transpose(transpose(M)), M).

to_row_vecs_test() ->
  M  = from_list([[1,2,3],[4,5,6],[7,8,9]]),
  Vs = [vector:from_binary(<<1,2,3>>),
        vector:from_binary(<<4,5,6>>),
        vector:from_binary(<<7,8,9>>)],
  ?assertEqual(Vs, to_row_vecs(M)).

% get_line_vector_test() ->
%   M = [[1,4,7],[2,5,8],[3,6,9]],
%   ?assertEqual(get_line_vector(M, 0), [1,4,7]).

% get_column_vector_test() ->
%   M = [[1,4,7],[2,5,8],[3,6,9]],
%   ?assertEqual(get_column_vector(M, 0), [1,2,3]).

-endif.