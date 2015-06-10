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
  Board = read_board_from_stdin(),
  SG  = ek_samegame:new(Board),
  Pop = ek_population:new(ek_gameboard:width(Board), ek_gameboard:height(Board)),
  loop(Pop, SG),
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

loop(Pop, SG) ->
  %%io:format("Calculating fitness~n"),
  PopFitness = ek_population:calculate_fitness(Pop, SG),
  %%io:format("Calculating next generation~n"),
  PopNext    = ek_population:next_generation(PopFitness),
  %%io:format("Looping~n"),
  loop(PopNext, SG).


