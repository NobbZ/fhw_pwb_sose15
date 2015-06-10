-module(erlking_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    queue()
  ]}}.

queue() ->
  {erlking_queue, {erlking_queue,  start_link, []}, permanent, 5000, worker, [erlking_queue]}.

old_init([]) ->
  Cores   = erlang:system_info(logical_processors_available),
  ListOfNames = lists:map(fun(A) ->
    list_to_atom(lists:flatten(io_lib:format("worker~w", [A])))
  end, lists:seq(1, 50 * Cores)),
  Workers = lists:map(fun(Label) ->
    {Label, {erlking_worker, start_link, []}, permanent, 5000, worker, [erlking_worker]}
  end, ListOfNames),
  Queue   = {queue_server, {erlking_queue,  start_link, []}, permanent, 5000, worker, [erlking_queue]},
  ProcessList = [Queue | Workers],
  {ok, { {one_for_one, 5, 10}, ProcessList} }.
