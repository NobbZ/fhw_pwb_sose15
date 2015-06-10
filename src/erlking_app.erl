-module(erlking_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  %wpool:start(),
  wpool:start_sup_pool(erlking_pool, [{worker, {erlking_worker, []}},
                                      {workers, 50}]),
  wpool:start_sup_pool(erlking_low_pool, [{worker, {erlking_worker, []}},
                                          {workers, 100}]),
  erlking_sup:start_link().
 % go_background().

stop(_State) ->
    ok.

% go_background() ->
%   timer:sleep(10 * 1000),
%   go_background().