-module (erlking_pool). 

-behaviour (gen_server).

-export ([add_job/1]).

%-define (STRATEGY, best_worker).
% -define (STRATEGY, random_worker).
 -define (STRATEGY, next_worker).
% -define (STRATEGY, available_worker).

add_job({low, Job}) ->
  wpool:cast(erlking_low_pool, Job, ?STRATEGY);
add_job(Job) ->
  %io:format("Distributing Job: ~w~n", [Job]),
  %lager:info("~w has ~w messages waiting.~n", [self(), erlang:process_info(self(), messages_queue_len)]),
  wpool:cast(erlking_pool, Job, ?STRATEGY).