-module(erlking_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(StartType, StartArgs) -> Result when
      StartType :: applicatio:start_type(),
      StartArgs :: term(),
      Result    :: {ok, pid()}
                 | {ok, pid(), State}
                 | {error, Reason},
      Reason    :: term(),
      State     :: term().
start(normal, _StartArgs) ->
  wpool:start_sup_pool(erlking_pool, [{worker, {erlking_worker, []}},
                                      {workers, 4}]),
  % wpool:start_sup_pool(erlking_low_pool, [{worker, {erlking_worker, []}},
  %                                         {workers, 100}]),
  erlking_sup:start_link().

-spec stop(term()) -> ok.
stop(_) ->
    ok.

