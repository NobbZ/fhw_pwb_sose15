%% @doc
%% @author Norbert Melzer <inf100760@fh-wedel.de>
%% @reference See <a href="http://en.wikipedia.org/wiki/Matrix_(mathematics)">
%% Wikipedia article about matrixes</a> for further information.
-module (matrix).

-export ([transpose/1, get_line_vector/2, get_column_vector/2, num_rows/1,
          num_cols/1, matrix/3]).
-export_type ([t/0, t/1]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type t()         :: t(any()).
-type t(A)        :: [[A]].
-type generator() :: fun((non_neg_integer(), non_neg_integer()) -> any()).

% ==============================================================================
% Retrieve information about the matrix
% ==============================================================================

%% @doc Retrieves the number of rows in a given matrix.
-spec num_rows(t()) -> pos_integer().
num_rows(M) ->
  lists:foldl(fun(_, Len) -> 1 + Len end, 0, M).

%% @doc Retrieves the mumber of columns in a given matrix.
-spec num_cols(t()) -> pos_integer().
num_cols(M) ->
  lists:max(lists:map(fun(E) ->
    lists:foldl(fun(_, Len) ->
      1 + Len
    end, 0, E)
  end, M)).

% ==============================================================================
% Builders
% ==============================================================================

%% @doc Generate a matrix from a generator function.
-spec matrix(pos_integer(), pos_integer(), generator()) -> t().
matrix(Rows, Cols, GenFun) ->
  A = lists:sort(lists:flatten(lists:duplicate(Cols, lists:seq(0, Rows - 1)))),
  B = lists:flatten(lists:duplicate(Rows, lists:seq(0, Cols - 1))),
  C = lists:zipwith(GenFun, A, B),
  splice_list(C, Cols).

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
transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

% ==============================================================================
% Retrieve subvectors
% ==============================================================================

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

num_rows_test() ->
  ?assertEqual(num_rows([[1,2,3],[4,5,6],[7,8,9]]), 3),
  ?assertEqual(num_rows([[1,2,3]]), 1).

num_cols_test() ->
  ?assertEqual(num_cols([[1,2,3],[4,5,6],[7,8,9]]), 3),
  ?assertEqual(num_cols([[1,2,3]]), 3).

matrix_test() ->
  ?assertEqual(matrix(3, 3, fun(X,Y) -> (X+1) * (Y+1) end), [[1,2,3],[2,4,6],[3,6,9]]).

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