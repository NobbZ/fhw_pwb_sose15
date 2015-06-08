%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2015 22:06
%%%-------------------------------------------------------------------
-module(ek_samegame).
-author("Norbert Melzer").

-include("individual.hrl").

%% API
-export([new/1]).

-record(samegame, {colors = 0 :: non_neg_integer(),
                   history = nil :: ek_history:history(),
                   cols = 0 :: non_neg_integer(),
                   rows = 0 :: non_neg_integer(),
                   board = nil :: ek_gameboard:t() | nil}).

new(Board) ->
  Colors = count_colors(Board),
  Rows = ek_gameboard:height(Board),
  Cols = ek_gameboard:width(Board),
  History = ek_history:new(Colors, Cols, Rows),
  ek_history:set_initial(History, Board),
  #samegame{colors  = Colors,
            history = History,
            cols    = Cols,
            rows    = Rows,
            board   = Board}.

count_colors(Board) ->
  V0 = matrix:to_row_vecs(Board),
  V1 = lists:foldl(fun(V, Acc) -> vector:concat(Acc, V) end,
                   vector:from_binary(<<>>), V0),
  List = binary_to_list(vector:to_binary(V1)),
  Colors =
  lists:foldr(fun(X, Acc) -> sets:add_element(X, Acc) end, sets:new(), List),
  sets:size(Colors).

fitness_of(#samegame{} = Self, #individual{} = I) ->
  Start = ek_history:get_initial(Self#samegame.history),
  IdxList = lists:seq(1, length(I#individual.g)),
  GwithIdx = lists:zip(I#individual.g, IdxList),
  {NewI, _, _} = lists:foldl(fun iterate_g/2, {I, Self, false}, GwithIdx),
  NewI.

iterate_g({_, _}, {#individual{}, #samegame{}, true} = Res) -> Res;
iterate_g({HitIdx, Idx}, {#individual{} = I, #samegame{} = SG, false}) ->
  {Score, Next} = ek_history:make_click(SG#samegame.history, SG#samegame.board,
    HitIdx),
  NewI   = I#individual{f = I#individual.f + Score},
  NewSG  = SG#samegame{board = Next},
  Finished = Score == 0,
  {NewI, NewSG, Finished}.

