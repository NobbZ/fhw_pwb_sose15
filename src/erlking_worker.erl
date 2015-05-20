-module (erlking_worker).

-behaviour(gen_server).

%%% gen_server-exports
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
         terminate/2]).

%%% API-exports
-export ([]).

-spec handle_call(Request, From, State) -> Result when
      Request  :: term(),
      From     :: {pid(), term()},
      State    :: term(),
      Result   :: {noreply, NewState},
      NewState :: term().
handle_call(_, _, State) ->
    {noreply, State}.

-spec handle_cast(Request, State) -> Result when
      Request  :: term(),
      State    :: integer(),
      Result   :: {reply, NewState},
      NewState :: integer().
handle_cast(read_board, _) ->
    Board = io:get_line(""),
    BoardMtrx = gameboard:parse_board(Board),
    erlking_pool:add_job({analyze, BoardMtrx, []}),
    {noreply, 0};
handle_cast({analyze, Board, History}, S) ->
                                                %lager:info("{~w, ~w, ~w}, ~w", [analyze, Board, History, S]),
    Moves = gameboard:find_clickables(Board),
                                                %lager:info("~w", [Moves]),
    lists:map(fun ({Move, Potential}) ->
                      if Potential >= 3 -> erlking_pool:add_job({progress, Board, Move, History});
                         true -> nil end
              end, Moves),
    {noreply, S};
handle_cast({progress, Board, Move, History} = _Job, S) ->
%%%{heap_size, Mem} = process_info(self(), heap_size),
    {message_queue_len, Len} = process_info(self(), message_queue_len),
                                                %lager:info("~w has a Heap of ~w~n", [self(), Mem]),
                                                %case Mem >= 50000 of
    case Len >= 10000 of
        true ->
                                                %lager:info("~w dropped job '~w'~n", [self(), Job]),
            {noreply, S};
        false ->
                                                %lager:info("S is ~w~n", [S]),
                                                %lager:info("~w has ~w messages waiting.~n", [self(), erlang:process_info(self(), message_queue_len)]),
            LastScore = case History of
                [{_, LS}|_] -> LS;
                []          -> 0
            end,
            {NewBoard, Score} = gameboard:makemove(Board, Move),
            NewScore = Score + LastScore,
            NewHistory = [{Move, NewScore}|History],
            NewState = if NewScore > S -> erlking_queue:result(NewHistory, Score + LastScore);
                          true         -> S
                       end,
%%%%%%% erlking_pool:add_job({analyze, NewBoard, NewHistory}),
            AMoves = gameboard:find_clickables(NewBoard),
            lists:map(fun ({SendMove, Potential}) ->
                              if Potential >= 1 -> erlking_pool:add_job({progress, NewBoard, SendMove, NewHistory});
                                  true -> erlking_pool:add_job({low, {progress, NewBoard, SendMove, NewHistory}}) end
                      end, AMoves),
            {noreply, NewState}
    end.

-spec handle_info(Info, State) -> Result when
      Info     :: timeout | term(),
      State    :: term(),
      Result   :: {noreply, NewState},
      NewState :: term().
handle_info(_, State) ->
    {noreply, State}.

-spec init(Args) -> Result when
      Args :: term(),
      Result :: {ok, 0}.
init(_) ->
    lager:info("Worker started, ~w~n", [self()]),
    {ok, 0}.

-spec terminate(Reason, State) -> term() when
      Reason :: normal
              | shutdown
              | {shutdown, term()}
              | term(),
      State  :: term().
terminate(normal, State) ->
    lager:info("~w shut down normally with state ~w~n", [self(), State]);
terminate(shutdown, State) ->
    lager:info("~w shut down because of shutdown with state ~w~n",
               [self(), State]);
terminate({shutdown, Reason}, State) ->
    lager:info("~w shut down with reason ~w and state ~w~n",
               [self(), Reason, State]);
terminate(Reason, State) ->
    lager:info("~w shut down because of unknown reason ~w, state was ~w~n",
               [self(), Reason, State]).

-spec code_change(OldVsn, State, Extra) -> Result when
      OldVsn   :: Vsn
                | {down, Vsn},
      Vsn      :: term(),
      State    :: term(),
      NewState :: term(),
      Extra    :: term(),
      Reason   :: term(),
      Result   :: {ok, NewState}
                | {error, Reason}.
code_change(_Old, State, _Extra) ->
    {ok, State}.
