%% @doc
%% @author Norbert Melzer <inf100760@fh-wedel.de>
%% @reference See <a href="http://en.wikipedia.org/wiki/Matrix_(mathematics)">
%% Wikipedia article about matrixes</a> for further information.
-module (matrix).

-export ([transpose/1, get_line_vector/2, get_column_vector/2]).
-export_type ([t/0, t/1]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type t()    :: t(any()).
-type t(A)   :: [[A]].

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
transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

%% @doc Gets the specified line-vector.
%% Counting starts with 0.
-spec get_line_vector(t(), non_neg_integer()) -> vector:t().
get_line_vector(Matrix, Line) -> 
  lists:nth(Line + 1, Matrix).

%% @doc Gets the specified column-vector.
%% Counting starts with 0.
-spec get_column_vector(t(), non_neg_integer()) -> vector:t().
get_column_vector(Matrix, Column) ->
  get_line_vector(transpose(Matrix), Column).

% ==============================================================================
% Tests
% ==============================================================================

-ifdef (TEST).

transpose_1_x_m_test() ->
  ?assertEqual(transpose([[1,2,3]]), [[1],[2],[3]]).

transpose_n_x_1_test() ->
  ?assertEqual(transpose([[1],[2],[3]]), [[1,2,3]]).

transpose_n_x_m_test() ->
  ?assertEqual(transpose([[1,2,3],[4,5,6],[7,8,9]]), [[1,4,7],[2,5,8],[3,6,9]]).

transpose_transpose_is_id_test() ->
  M = [[1,4,7],[2,5,8],[3,6,9]],
  ?assertEqual(transpose(transpose(M)), M).

get_line_vector_test() ->
  M = [[1,4,7],[2,5,8],[3,6,9]],
  ?assertEqual(get_line_vector(M, 0), [1,4,7]).

get_column_vector_test() ->
  M = [[1,4,7],[2,5,8],[3,6,9]],
  ?assertEqual(get_column_vector(M, 0), [1,2,3]).

-endif.