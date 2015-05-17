-module(erlking_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  erlking_sup:start_link(),
  go_background().

stop(_State) ->
    ok.

go_background() ->
  timer:sleep(10 * 1000),
  go_background().