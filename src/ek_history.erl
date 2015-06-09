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
         restore_board/2, set_initial/2, get_initial/1, make_click/3, to_click_list/1]).

-define(LIMIT, 50000).

-include("individual.hrl").

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
  PossHits = ek_gameboard:find_clickables(Board),
  ets:insert(Self#history.history_of_possible_hits,
             {initial, Board, nil}),
  add_possible_hits(Self, initial, PossHits),
  Self.

get_initial(#history{} = Self) ->
  restore_board(Self, initial).

add_possible_hits(#history{} = Self, Board, PossibleHits)
  when is_list(PossibleHits) ->
  PreparedHits = lists:map(fun(X) -> {X, nil} end, PossibleHits),
  add_possible_hits(Self, Board, array:fix(array:from_list(PreparedHits)));
add_possible_hits(#history{} = Self, Board, PossibleHits) ->
  {BoardHash, BoardMtrx} = case ek_gameboard:is_gameboard(Board) of
    true ->
      BH = ek_gameboard:hash(Board),
      {BH, Board};
    false ->
      BM = ets:lookup_element(Self#history.history_of_possible_hits, Board, 2),
      {Board, BM}
  end,
  ets:insert(Self#history.history_of_possible_hits,
             {BoardHash, BoardMtrx, PossibleHits}),
  Self.

update_hits(#history{} = Self, Current, Hit, Next) ->
  F = fun(Item) ->
    case Item of
      {{C, P}, nil} ->
        case C == Hit of
          true -> {{C, P}, Next};
          false -> {{C, P}, nil}
        end;
      _ -> Item
    end
  end,
  %%io:format("looking up~n"),
  [{_, B, Hits}] = ets:lookup(Self#history.history_of_possible_hits, Current),
  %%io:format("Looked up ~p and ~p", [B, Hits]),
  NewHits = lists:map(F, array:to_list(Hits)),
  %%io:format("NewHits: ~p~n", [NewHits]),
  ArrHits = array:fix(array:from_list(NewHits)),
  ets:insert(Self#history.history_of_possible_hits, {Current, B, ArrHits}).

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

to_click_list(#individual{} = I) ->
  to_click_list_rec(I#individual.g, initial, []).

to_click_list_rec([], _, Acc) -> Acc;
to_click_list_rec([Head|Tail], BoardHash, Acc) ->
  %%io:format("Head: ~p; Tail: ~p~n  Acc: ~p~n", [Head, Tail, Acc]),
  %%io:format("Looking up ~p~n", [BoardHash]),
  Hit = case ets:lookup(history_of_possible_hits, BoardHash) of
          [{_, _, Hs}] ->
            case array:size(Hs) of
              0 -> nil;
              S -> NewIdx = Head rem S,
                array:get(NewIdx, Hs)
            end
  end,
  %%io:format("Hit: ~p~n", [Hit]),
  case Hit of
    {Move, NH} -> to_click_list_rec(Tail, NH, [Move|Acc]);
    nil -> Acc
  end.

make_click(#history{} = Self, Board, HitIdx) when not is_integer(Board) and
                                                  not (Board == initial) ->
  case ek_gameboard:is_gameboard(Board) of
    true -> make_click(Self, ek_gameboard:hash(Board), HitIdx);
    false -> error(badarg)
  end;
make_click(#history{} = Self, Board, HitIdx) ->
  %%io:format("Looking up BoardMtrx and Hit~n"),
  {BoardMtrx, Hit} = case ets:lookup(Self#history.history_of_possible_hits,
                                     Board) of
    [{_, BM, Hs}] ->
      %%io:format("~p, ~p~n", [BM, Hs]),
      H = case array:size(Hs) of
        0 -> nil;
        S ->
          NewIdx = HitIdx rem S,
          array:get(NewIdx, Hs)
      end,
      {BM, H};
    [] ->
      %%io:format("Got nothing"),
      {nil, nil}
      %%                 X -> io:format("Got ~p~n", [X])
  end,
  %%io:format("Clicking ~p on ~p (~w)~n", [Hit, Board, BoardMtrx]),
  case Hit of
    {{Coord, _}, nil} ->
      {NextBrd, ThisScore} = ek_gameboard:makemove(BoardMtrx, Coord),
      PossMoves = ek_gameboard:find_clickables(NextBrd),
      NextHash = ek_gameboard:hash(NextBrd),
      %%io:format("Updating Hits~n"),
      update_hits(Self, Board, Coord, NextHash),
      %%io:format("Adding possible hits to next~n"),
      add_possible_hits(Self, NextBrd, PossMoves),
      {ThisScore, NextHash};
    {{_, Stones}, NextHash} ->
      {(Stones - 1) * (Stones - 1), NextHash};
    nil ->
      Penalty = ek_gameboard:endgame(BoardMtrx),
      {Penalty, nil}
  end.
