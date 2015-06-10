-module(erlking_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _StartArgs) ->
  Cores = number_of_cores(),
  ek_supervisor:start_link(),
  {ok, self()}.

number_of_cores() ->
  case erlang:system_info(logical_processors_available) of
    unknown -> 1;
    C when C =< 0 -> 1;
    C -> C
  end.

stop(_) ->
  ok.

read_board_from_stdin() ->
  BoardString = io:get_line(""),
  ek_gameboard:parse_board(BoardString).




