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
    Cores = case erlang:system_info(logical_processors_available) of
                unknown       -> 1;
                C when C =< 0 -> 1;
                C             -> C
            end,
    wpool:start_sup_pool(erlking_low, [{worker, {erlking_worker, []}},
                                       {workers, 8 * Cores}]),
    wpool:start_sup_pool(erlking_mid, [{worker, {erlking_worker, []}},
                                       {workers, 4 * Cores}]),
    wpool:start_sup_pool(erlking_hi,  [{worker, {erlking_worker, []}},
                                       {workers, 2 * Cores}]),
    erlking_sup:start_link().

-spec stop(term()) -> ok.
stop(_) ->
    ok.

