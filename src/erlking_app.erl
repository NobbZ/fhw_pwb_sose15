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
    %% wpool:start_sup_pool(erlking_low, [{worker, {erlking_worker, []}},
    %%                                    {workers, 8 * Cores}]),
    %% wpool:start_sup_pool(erlking_mid, [{worker, {erlking_worker, []}},
    %%                                    {workers, 4 * Cores}]),
    %% wpool:start_sup_pool(erlking_hi,  [{worker, {erlking_worker, []}},
    %%                                    {workers, 2 * Cores}]),
    wpool:start_sup_pool(erlking_test, [{worker, {erlking_pworker, []}},
                                        {workers, 10 * Cores}]),
    erlking_sup:start_link(),
    timer:sleep(100),
    read_board_from_stdin_and_send_it_as_job(),
    {ok, self()}.

-spec stop(term()) -> ok.
stop(_) ->
    ok.

read_board_from_stdin_and_send_it_as_job() ->
    BoardString = io:get_line(""),
    BoardMtrx   = gameboard:parse_board(BoardString),
    Moves = gameboard:find_clickables(BoardMtrx),
    Jobs = lists:map(fun({ThisClick, Pot}) ->
                             #job{potential = Pot,
                                  board     = BoardMtrx,
                                  click     = ThisClick}
                     end, Moves),
    lists:map(fun(Job) ->
                      erlking_pool:add_job2(Job)
              end, Jobs).
