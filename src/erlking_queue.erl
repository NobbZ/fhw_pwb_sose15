-module (erlking_queue).
-export ([start/0, init/0, send_result/3, add_job/2, get_job/1]).

start() ->
  spawn(erlking_queue, init, []).

init() ->
  register(queue, self()),
  loop(0, []).

loop(MaxScore, JobQueue) ->
  receive
    {result, Moves, Score} when Score > MaxScore ->
      emit_moves_to_stdout(Moves),
      loop(Score, JobQueue);
    {result, _Moves, _Score} ->
      loop(MaxScore, JobQueue);
    {add_job, Job} ->
      loop(MaxScore, [Job|JobQueue]);
    {get_job, Pid} ->
      JobAvailable = JobQueue /= [],
      if
        JobAvailable ->
          Answer = {ok, lists:nth(1, JobQueue)},
          NewQueue = lists:nthtail(1, JobQueue);
        true ->
          Answer = no_job_available,
          NewQueue = JobQueue
      end,
      Pid ! Answer,
      loop(MaxScore, NewQueue)
  end.

send_result(Pid, Moves, Score) ->
  Pid ! {result, Moves, Score}.

add_job(Pid, Job) ->
  Pid ! {add_job, Job}.

get_job(Pid) ->
  Pid ! {get_job, self()},
  receive
    Answer -> Answer
  end.

emit_moves_to_stdout(Moves) ->
  io:format("[", []),
  emit_them_entirely(Moves),
  io:format("]~n", []).

emit_them_entirely([]) ->
  io:format("", []);
emit_them_entirely([{X, Y}|[]]) ->
  io:format("(~w, ~w)", [X, Y]);
emit_them_entirely([{X, Y}|Moves]) ->
  io:format("(~w,~w),", [X, Y]),
  emit_them_entirely(Moves).
