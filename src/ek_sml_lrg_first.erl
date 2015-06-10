%%%-------------------------------------------------------------------
%%% @author Norbert Melzer <inf100760@fh-wedel.de>
%%% @copyright 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 01. Jun 2015 15:24
%%%-------------------------------------------------------------------
-module(ek_sml_lrg_first).
-author("Norbert Melzer").

-include("job.hrl").

%% API
-export([run_board/4]).

%%% @todo Whitespace!
run_board(Board, History, none, Score) ->
  lager:info("started Small/Large"),
  Moves = ek_gameboard:find_clickables(Board),
  {Smallest, Largest, Other} = get_worksets(Moves),
  ParallelWorkSet = Smallest ++ Largest,
  do_parallel_work(Board, History, Score, ParallelWorkSet, 10),
  send_remaining_jobs(Board, History, Score, Other),
  ek_proxy:run(),
  lager:info("First finished").

run_board(_, _, _, _, 0) -> ok;
run_board(Board, History, Click, Score, Depth) ->
  {NewBoard, NewScore} = ek_gameboard:makemove(Board, Click),
  InterScore = Score + NewScore,
  NewHistory = [Click|History],
  Moves = ek_gameboard:find_clickables(NewBoard),
  EndScore = InterScore + ek_gameboard:endgame(NewBoard),
  %lager:info("EndScore: ~p", [EndScore]),
  ek_queue:result(NewHistory, EndScore),
  {Smallest, Largest, Other} = get_worksets(Moves),
  ParallelWorkset = Smallest ++ Largest,
  do_parallel_work(NewBoard, NewHistory, EndScore, ParallelWorkset, Depth - 1),
  send_remaining_jobs(NewBoard, NewHistory, InterScore, Other).
  %lager:info("Deepworker (~p) finished", [Depth]).


send_remaining_jobs(NewBoard, NewHistory, InterScore, Other) ->
  NewBoardHash = erlang:now(),
  ets:insert(jobstore, {NewBoardHash, NewBoard}),
  RemainingJobs = lists:map(fun({ThisClick, P}) ->
    #job{
      potential = P,
      board     = NewBoardHash,
      click     = ThisClick,
      history   = NewHistory,
      lastscore = InterScore
    } end,                  Other),
  pmap(fun(RJob) -> ek_pool:add_job(RJob) end, RemainingJobs).

do_parallel_work(NewBoard, NewHistory, EndScore, ParallelWorkset, Depth) ->
  F = fun({C, _}) -> run_board(NewBoard, NewHistory, C, EndScore, Depth) end,
  pmap(F, ParallelWorkset).

get_worksets([]) -> {[], [], []};
get_worksets(Moves) ->
  MovesSmallestFirst = lists:sort(fun small_first_order/2, Moves),
  MovesLargestFirst = lists:reverse(MovesSmallestFirst),
  [{_, SmallPot}|_] = MovesSmallestFirst,
  [{_, LargePot}|_] = MovesLargestFirst,
  Smallest = lists:takewhile(fun({_, E}) -> E == SmallPot end, MovesSmallestFirst),
  Largest = lists:takewhile(fun({_, E}) -> E == LargePot end, MovesLargestFirst),
  OtherPre = lists:reverse(lists:dropwhile(fun({_, E}) -> E == SmallPot end, MovesSmallestFirst)),
  Other = lists:dropwhile(fun({_, E}) -> E == LargePot end, OtherPre),
  {Smallest, Largest, Other}.

small_first_order({_, P1}, {_, P2}) ->
  P1 =< P2.

split_list(List) ->
  split_list(List, {[], [], []}).

split_list([], {Min, Min, Other}) -> {[], Min, Other};
split_list([], Acc) -> Acc;
split_list([{_, _} = I|Tail], {[], [], []}) ->
  split_list(Tail, {[I], [I], []});
split_list([{_, P} = I|Tail], {[{_, P}|_] = Min, [{_, P}|_] = Min, Other}) ->
  split_list(Tail, {[I|Min], [I|Min], Other});
split_list([{_, P} = I|Tail], {[{_, P}|_] = Min, Max, Other}) ->
  split_list(Tail, {[I|Min], Max, Other});
split_list([{_, P} = I|Tail], {Min, [{_, P}|_] = Max, Other}) ->
  split_list(Tail, {Min, [I|Max], Other});
split_list([{_, P} = I|Tail], {[{_, MinP}|_] = Min, Min, Other}) when P < MinP ->
  split_list(Tail, {[I], Min, Other});
split_list([{_, P} = I|Tail], {Max, [{_, MaxP}|_] = Max, Other}) when P > MaxP ->
  split_list(Tail, {Max, [I], Other});
split_list([{_, P} = I|Tail], {[{_, MinP}|_] = Min, Max, Other}) when P < MinP ->
  split_list(Tail, {[I], Max, lists:append(Min, Other)});
split_list([{_, P} = I|Tail], {Min, [{_, MaxP}|_] = Max, Other}) when P > MaxP ->
  split_list(Tail, {Min, [I], lists:append(Max, Other)});
split_list([{_, P} = I|Tail], {Min, Max, Other}) ->
  split_list(Tail, {Min, Max, [I|Other]}).

pmap(F, List) ->
  S = self(),
  Pids = lists:map(fun(E1) ->
    spawn(fun() -> execute(S, F, E1) end)
  end, List).
  %%gather(Pids).

execute(Recv, F, E) ->
  Recv ! {self(), F(E)}.

gather([]) -> [];
gather([X|Xs]) ->
  receive
    {X, Ret} -> [Ret|gather(Xs)]
  end.