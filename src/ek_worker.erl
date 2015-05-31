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
  ek_proxy:ask_for_job(Proxy),
  State1 = receive
             Job ->
               {Jobs, Score, Hist} = process_job(Job),
               NewLast = tell_score(Score, Hist, Last),
               lists:map(fun(#job{} = FunJob) ->
                 ek_pool:add_job(FunJob)
               end, Jobs),
               State#state{lastscore = NewLast}
           after 10 -> State
           end,
  loop(State1).

tell_score(Score, History, LastScore) ->
  case Score > LastScore of
    true -> ek_queue:result(History, Score);
    false -> LastScore
  end.

process_job(#job{potential = Pot,
  board = Board,
  click = Click,
  history = History,
  lastscore = Last,
  whitespace = WS}) ->
  {NewBoard, JobScore} = ek_gameboard:makemove(Board, Click),
  InterScore = Last + JobScore,
  NewHistory = [Click | History],
  Moves = ek_gameboard:find_clickables(NewBoard),
  %% In der Theorie sinkt die Penalty jedesmal um den Betrag des letzten Klicks
  %% EndScore = InterScore + JobScore, %% InterScore + gameboard:endgame(NewBoard),
  %% Aber das funktioniert doch nicht :( Muss ich weiter überlegen wie ich mir das einsparen kann
  EndScore = InterScore + ek_gameboard:endgame(NewBoard),
  Jobs = lists:map(fun({ThisClick, P}) ->
    #job{potential = P,
      board = NewBoard,
      click = ThisClick,
      history = NewHistory,
      lastscore = InterScore,
      %%lastscore = EndScore,
      whitespace = WS + Pot}
  end, Moves),
  {Jobs, EndScore, NewHistory}.