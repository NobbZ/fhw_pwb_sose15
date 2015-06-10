-module (erlking_queue).
-behaviour (gen_server).

% API
-export ([start_link/0, get_job/0, add_job/1]).

% Exports for implementing behaviour 'gen_server'.
-export ([code_change/3,
          handle_cast/2,
          handle_call/3,
          handle_info/2,
          init/1]).

start_link() ->
  gen_server:start_link({global, ek_queue}, erlking_queue, [], []).

get_job() ->
  gen_server:call(ek_queue, get_job).

add_job(Job) ->
  gen_server:cast(ek_queue, {add_job, Job}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
  {ok, {0, heaps:new()}, infinity}.

code_change(_, _, _) ->
  {error, "No updates supported!"}.

handle_call(get_job, _From, {MaxScore, JobQueue}) ->
  IsAvailable = not heaps:is_empty(JobQueue),
  if IsAvailable -> {Job, NewQueue} = heaps:fetch(JobQueue),
                    {reply, Job, {MaxScore, NewQueue}};
     true -> {reply, no_job, {MaxScore, JobQueue}}
  end.
  
handle_cast({result, Moves, Score}, {MaxScore, JobQueue}) when Score > MaxScore ->
  emit_moves_to_stdout(Moves),
  {noreply, {Score, JobQueue}};
handle_cast({result, _, _}, State) -> {noreply, State};
handle_cast({add_job, Job}, {MaxScore, JobQueue}) -> 
  io:format("Added job: ~w~n", [Job]),
  {noreply, {MaxScore, heaps:add(JobQueue, Job)}}.

handle_info(_, State) -> {noreply, State}.

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
