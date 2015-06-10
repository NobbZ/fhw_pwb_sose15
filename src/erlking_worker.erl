-module (erlking_worker).

-export ([start_link/0, loop/0, init/1, handle_cast/2]).

start_link() ->
  Pid = spawn_link(?MODULE, loop, []),
  io:format("Started worker with PID ~w.~n", [Pid]),
  {ok, Pid}.

loop() ->
  %io:format("~w is in the loop~n", [self()]),
  Job = erlking_queue:get_job(),
  %io:format("Got job: ~w for ~w.~n", [Job, self()]),
  case Job of
    read_board ->
      Board = io:get_line(""),
      BoardMtrx = gameboard:parse_board(Board),
      erlking_queue:add_job({analyze, BoardMtrx, []});
    {analyze, Board, History} ->
      Moves = gameboard:find_clickables(Board),
      lists:map(fun (Move) ->
        erlking_queue:add_job({progress, Board, Move, History})
      end, Moves);
    {progress, Board, Move, History} ->
      case History of
        [{_, LS}|_] -> LastScore = LS;
        []          -> LastScore = 0
      end,
      {NewBoard, Score} = gameboard:makemove(Board, Move),
      NewHistory = [{Move, Score + LastScore}|History],
      erlking_queue:result(NewHistory, Score + LastScore),
      erlking_queue:add_job({analyze, NewBoard, NewHistory});
    no_job ->
      %io:format("Pausing ~w~n", [self()]),
      timer:sleep(10)
  end,
  loop().

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
handle_cast({progress, Board, Move, History}, S) ->
  %lager:info("S is ~w~n", [S]),
  %lager:info("~w has ~w messages waiting.~n", [self(), erlang:process_info(self(), message_queue_len)]),
  case History of
    [{_, LS}|_] -> LastScore = LS;
    []          -> LastScore = 0
  end,
  {NewBoard, Score} = gameboard:makemove(Board, Move),
  NewScore = Score + LastScore,
  NewHistory = [{Move, NewScore}|History],
  NewState = if NewScore > S -> erlking_queue:result(NewHistory, Score + LastScore);
                true         -> S
  end,
  %%%%%%% erlking_pool:add_job({analyze, NewBoard, NewHistory}),
  AMoves = gameboard:find_clickables(NewBoard),
  lists:map(fun ({Move, Potential}) ->
    if Potential >= 3 -> erlking_pool:add_job({progress, NewBoard, Move, NewHistory});
       true -> erlking_pool:add_job({low, {progress, NewBoard, Move, NewHistory}}) end
  end, AMoves),
  {noreply, NewState}.


init(_) ->
  io:format("Worker started, ~w~n", [self()]),
  {ok, 0}.