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
-export([run_board/5]).

%%% @todo Whitespace!
run_board(Board, History, Click, Score, Initial) ->
  {NewBoard, NewScore} = ek_gameboard:makemove(Board, Click),
  InterScore = Score + NewScore,
  NewHistory = [Click|History],
  Moves = ek_gameboard:find_clickables(NewBoard),
  EndScore = InterScore + ek_gameboard:endgame(NewBoard),
  ek_queue:result(NewHistory, EndScore),
  {Smallest, Largest, Other} = split_list(Moves),
  ParallelWorkset = Smallest ++ Largest,
  F = fun({C, _}) -> run_board(NewBoard, NewHistory, C, EndScore, false) end,
  pmap(F, ParallelWorkset),
  RemainingJobs = lists:map(fun({ThisClick, P}) ->
    #job{
      potential = P,
      board = NewBoard,
      click = ThisClick,
      history = NewHistory,
      lastscore = InterScore
    } end, Other),
  lists:map(fun(RJob) ->
    ek_pool:add_job(RJob)
  end, RemainingJobs),
  if Initial -> ek_proxy:run() end.

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
  end, List),
  gather(Pids).

execute(Recv, F, E) ->
  Recv ! {self(), F(E)}.

gather([]) -> [];
gather([X|Xs]) ->
  receive
    {X, Ret} -> [Ret|gather(Xs)]
  end.