-module(ek_worker).

-export([start_link/1]).

-record(state, {proxy = undefined :: port() | undefined,
  lastscore = -4294836225 :: integer()}).

-include("job.hrl").

-spec start_link(pid()) -> no_return().
start_link(Proxy) ->
  State = #state{proxy = Proxy},
  loop(State).

loop(#state{proxy = Proxy, lastscore = Last} = State) ->
  ek_proxy:ask_for_job(Proxy, self()),
  State1 = receive
             no_job ->
               State;
             Job ->
               {Jobs, Score, Hist} = process_job(Job),
               NewLast = tell_score(Score, Hist, Last),
               lists:map(fun(#job{} = FunJob) ->
                 ek_pool:add_job(FunJob)
               end, Jobs),
               State#state{lastscore = NewLast}
           end,
  loop(State1).

tell_score(Score, History, LastScore) ->
  case Score > LastScore of
    true -> ek_queue:result(History, Score);
    false -> LastScore
  end.

process_job(#job{potential = Pot,
  board = BoardHash,
  click = Click,
  history = History,
  lastscore = Last,
  whitespace = WS}) ->
  Board = ets:lookup_element(jobstore, BoardHash, 2),
  {NewBoard, JobScore} = ek_gameboard:makemove(Board, Click),
  NewBoardHash = erlang:now(),
  ets:insert(jobstore, {NewBoardHash, NewBoard}),
  InterScore = Last + JobScore,
  NewHistory = [Click | History],
  Moves = ek_gameboard:find_clickables(NewBoard),
  %% In der Theorie sinkt die Penalty jedesmal um den Betrag des letzten Klicks
  %% EndScore = InterScore + JobScore, %% InterScore + gameboard:endgame(NewBoard),
  %% Aber das funktioniert doch nicht :( Muss ich weiter überlegen wie ich mir das einsparen kann
  EndScore = InterScore + ek_gameboard:endgame(NewBoard),
  Jobs = lists:map(fun({ThisClick, P}) ->
    #job{potential = P,
      board = NewBoardHash,
      click = ThisClick,
      history = NewHistory,
      lastscore = InterScore,
      %%lastscore = EndScore,
      whitespace = WS + Pot}
  end, Moves),
  JobsHeap = heaps:from_list(Jobs, fun ek_proxy:compare/2),
  JobList = take(5, heaps:to_list(JobsHeap)),
  {JobList, EndScore, NewHistory}.

take(0, _) -> [];
take(_, []) -> [];
take(N, [X|Xs]) -> [X|take(N-1, Xs)].