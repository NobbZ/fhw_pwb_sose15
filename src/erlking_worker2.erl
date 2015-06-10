-module(erlking_worker2).

-export([start_link/1]).

-record(state, {proxy     = undefined   :: port() | undefined,
                lastscore = -4294836225 :: integer()}).

-include("job.hrl").

start_link(Proxy) ->
    State = #state{proxy = Proxy},
    loop(State).

loop(#state{proxy = Proxy, lastscore = Last} = State) ->
    erlking_proxy:ask_for_job(Proxy),
    State1 = receive
                 Job ->
                     {Jobs, Score, Hist} = process_job(Job),
                     NewLast = tell_score(Score, Hist, Last),
                     lists:map(fun(#job{potential = Pot} = FunJob) ->
                                       erlking_pool:add_job2(FunJob)
                               end, Jobs),
                     State#state{lastscore = NewLast}
             after 10 -> State
             end,
    loop(State1).

tell_score(Score, History, LastScore) ->
    case Score > LastScore of
        true  -> erlking_queue:result2(History, Score);
        false -> LastScore
    end.

process_job(#job{board = Board,
                 click = Click,
                 history = History,
                 lastscore = Last}) ->
    {NewBoard, JobScore} = gameboard:makemove(Board, Click),
    InterScore = Last + JobScore,
    NewHistory = [Click|History],
    Moves = gameboard:find_clickables(NewBoard),
    EndScore = InterScore + gameboard:endgame(NewBoard),
    Jobs = lists:map(fun({ThisClick, Pot}) ->
                             #job{potential = Pot,
                                  board     = NewBoard,
                                  click     = ThisClick,
                                  history   = NewHistory,
                                  lastscore = InterScore}
                     end, Moves),
    {Jobs, EndScore, NewHistory}.
