-module(erlking_app).

-behaviour(application).

-include("job.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(StartType, StartArgs) -> Result when
  StartType :: applicatio:start_type(),
  StartArgs :: term(),
  Result :: {ok, pid()}
  | {ok, pid(), State}
  | {error, Reason},
  Reason :: term(),
  State :: term().
start(normal, _StartArgs) ->
  Cores = case erlang:system_info(logical_processors_available) of
            unknown -> 1;
            C when C =< 0 -> 1;
            C -> C
          end,
  ets:new(jobstore, [set, public, named_table,
    %% compressed,
    {keypos, 1}]),
  ets:new(colorstore, [set, public, {keypos, 1}, named_table]),
  wpool:start_sup_pool(erlking_test, [{worker, {ek_pworker, []}},
    {workers, 1 * Cores}]),
  ek_supervisor:start_link(),
  timer:sleep(100),
  read_board_from_stdin_and_send_it_as_job(),
  {ok, self()}.

-spec stop(term()) -> ok.
stop(_) ->
  ok.

read_board_from_stdin_and_send_it_as_job() ->
  BoardString = io:get_line(""),
  BoardMtrx = ek_gameboard:parse_board(BoardString),
  Moves = ek_gameboard:find_clickables(BoardMtrx),
  Whitespace = countwhite(BoardMtrx),
  ColorPercentages = get_color_percentages(BoardMtrx),
  ets:insert(colorstore, ColorPercentages),
  Jobs = lists:map(fun({ThisClick, Pot}) ->
    #job{potential = Pot,
      board = BoardMtrx,
      click = ThisClick,
      %%lastscore  = Penalty,
      whitespace = Whitespace}
  end, Moves),
  lists:map(fun(Job) ->
    ek_pool:add_job(Job)
  end, Jobs).

countwhite(Board) ->
  Vs = matrix:to_row_vecs(Board),
  V = lists:foldl(fun(Vec, Acc) -> vector:concat(Acc, Vec) end, vector:from_binary(<<>>), Vs),
  List = binary_to_list(vector:to_binary(V)),
  Zeros = lists:filter(fun(E) -> E == 0 end, List),
  length(Zeros).

get_color_percentages(Board) ->
  Vs = matrix:to_row_vecs(Board),
  V = lists:foldl(fun(Vec, Acc) -> vector:concat(Acc, Vec) end, vector:from_binary(<<>>), Vs),
  List = binary_to_list(vector:to_binary(V)),
  Stones = lists:foldl(fun(E, Store) -> ek_gameboard:inc_key(E, Store) end, [], List),
  Stones1 = lists:keydelete(0, 1, Stones),
  Size = matrix:get_height(Board) * matrix:get_width(Board),
  lists:keymap(fun(Val) -> Val / Size end, 2, Stones1).

