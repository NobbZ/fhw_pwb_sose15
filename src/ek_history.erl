%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2015 20:47
%%%-------------------------------------------------------------------
-module(ek_history).
-author("Norbert Melzer").

%% API
-export([new/3, add_possible_hits/3, possible_hits/2, hit_checked/3,
         restore_board/2, set_initial/2, get_initial/1, make_click/3]).

-define(LIMIT, 50000).

-record(history, {color_count = 0 :: non_neg_integer(),
                  col_count = 0 :: non_neg_integer(),
                  row_count = 0 :: non_neg_integer(),
                  history_of_possible_hits = nil :: nil | ets:tid(),
                  history_of_hit_checked = nil :: nil | ets:tid(),
                  limit = ?LIMIT :: non_neg_integer()}).

new(Colors, Cols, Rows) ->
  Hoph = ets:new(history_of_possible_hits, [set,
                                            public,
                                            named_table,
                                            {keypos, 1},
                                            {heir, none}]),
  Hohc = ets:new(history_of_hit_checked, [set,
                                          public,
                                          named_table,
                                          {keypos, 1},
                                          {heir, none}]),
  #history{color_count              = Colors,
           col_count                = Cols,
           row_count                = Rows,
           history_of_hit_checked   = Hohc,
           history_of_possible_hits = Hoph}.

set_initial(#history{} = Self, Board) ->
  ets:insert(Self#history.history_of_possible_hits,
             {initial, Board, []}),
  Self.

get_initial(#history{} = Self) ->
  restore_board(Self, initial).

add_possible_hits(#history{} = Self, Board, PossibleHits)
  when is_list(PossibleHits) ->
  PreparedHits = lists:map(fun(X) -> {X, nil} end, PossibleHits),
  add_possible_hits(Self, Board, array:fix(array:from_list(PreparedHits)));
add_possible_hits(#history{} = Self, Board, PossibleHits) ->
  BoardHash = ek_gameboard:hash(Board),
  ets:insert(Self#history.history_of_possible_hits,
             {BoardHash, Board, PossibleHits}),
  Self.

possible_hits(#history{} = Self, Board) ->
  BoardHash = ek_gameboard:hash(Board),
  case ets:lookup(Self#history.history_of_possible_hits, BoardHash) of
    [{_, _, Hits}] -> array:size(Hits);
    [] -> nil
  end.

hit_checked(#history{} = Self, Board, HitIdx) ->
  case possible_hits(Self, Board) of
    nil -> nil;
    Hits -> array:get(HitIdx, Hits)
  end.

restore_board(#history{} = Self, BoardHash) ->
  [{_, Board, _}] = ets:lookup(Self#history.history_of_possible_hits,
                               BoardHash),
  Board.

make_click(#history{} = Self, Board, HitIdx) when not is_integer(Board) ->
  case ek_gameboard:is_gameboard(Board) of
    true -> make_click(Self, ek_gameboard:hash(Board), HitIdx);
    false -> error(badarg)
  end;
make_click(#history{} = Self, Board, HitIdx) when is_integer(Board) ->
  [{_, BoardMtrx, Hits}] = ets:lookup(Self#history.history_of_possible_hits,
                                      Board),
  Hit = array:get(HitIdx, Hits),
  case Hit of
    {{Coord, _}, nil} ->
      {NextBrd, ThisScore} = ek_gameboard:makemove(BoardMtrx, Coord),
      PossMoves = ek_gameboard:find_clickables(NextBrd),
      NextHash = ek_gameboard:hash(NextBrd),
      add_possible_hits(Self, NextBrd, PossMoves),
      {ThisScore, NextHash};
    {{_, Stones}, NextHash} ->
      {(Stones - 1) * (Stones - 1), NextHash}
  end.
