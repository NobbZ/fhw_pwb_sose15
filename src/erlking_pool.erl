-module (erlking_pool). 

-export ([add_job2/1]).

%-define (STRATEGY, best_worker).
% -define (STRATEGY, random_worker).
 -define (STRATEGY, next_worker).
% -define (STRATEGY, available_worker).

%% -spec add_job(any()) -> ok.
%% add_job({progress, Board, Weight, Move, History}) ->
%%     Pool = if Weight >= 7 -> erlking_hi;
%%               Weight >= 4 -> erlking_mid;
%%               true        -> erlking_low
%%            end,
%%     wpool:cast(Pool, {progress, Board, Move, History}, ?STRATEGY);
%% add_job(Job) ->
  %io:format("Distributing Job: ~w~n", [Job]),
  %lager:info("~w has ~w messages waiting.~n", [self(), erlang:process_info(self(), messages_queue_len)]),
  %wpool:cast(erlking_low, Job, ?STRATEGY).

add_job2(Job) ->
    wpool:cast(erlking_test, {add_job, Job}, ?STRATEGY).
