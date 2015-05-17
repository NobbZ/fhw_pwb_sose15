-module (erlking_worker).

-export ([start_link/0, loop/0]).

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