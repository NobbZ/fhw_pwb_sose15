-module (erlking_queue).
-behaviour (gen_server).

% API
-export ([start_link/0, get_job/0, add_job/1, get_info_to_terminal/0, compare_jobs/2,
          result/2, handle_call/3]).

% Exports for implementing behaviour 'gen_server'.
-export ([code_change/3,
          handle_cast/2,
          %handle_call/3,
          handle_info/2,
          init/1]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_job() ->
  % io:format("~w requested a job, calling gen_server!~n", [self()]),
  Res = gen_server:call(?MODULE, get_job),
  % io:format("This is ~w after gen_sever:call, Res = ~w", [self(), Res]),
  Res.

add_job(Job) ->
  gen_server:cast(?MODULE, {add_job, Job}).

result(Moves, Score) ->
  gen_server:call(?MODULE, {result, Moves, Score}).

get_info_to_terminal() ->
  gen_server:cast(?MODULE, print_info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_jobs(read_board, _) -> lt; % Read board from terminal is highes priority ever!
compare_jobs(J1, J2) -> % Fall back to default ordering if we didn't had a match before
  if J1 <  J2 -> lt;
     J1 == J2 -> eq;
     J1 >  J2 -> gt
  end.


init(_) ->
  EmptyQueue   = heaps:new(fun compare_jobs/2),
  %InitialQueue = heaps:add(EmptyQueue, read_board),
  % io:format("InitialQueue: ~w~n", [heaps:peek(InitialQueue)]),
  erlking_pool:add_job(read_board),
  {ok, 0, infinity}.

code_change(_, _, _) ->
  {error, "No updates supported!"}.

% handle_call(get_job, _From, MaxScore) ->
%   IsAvailable = not heaps:is_empty(JobQueue),
%   if IsAvailable -> {Job, NewQueue} = heaps:fetch(JobQueue),
%                     % io:format("~w requested a job, serving ~w~n", [_From, Job]),
%                     Reply = {reply, Job, {MaxScore, NewQueue}};
%      true -> Reply = {reply, no_job, {MaxScore, JobQueue}}
%   end,
%   % io:format("I want to reply with the reply '~w'!~n", [Reply]),
%   Reply.

handle_call({result, Moves, Score}, _From, MaxScore) when Score > MaxScore ->
  %lager:info("New score of ~w emitted, old score was ~w, new history is ~w~n", [Score, MaxScore, Moves]),
  lager:info("New Score: ~w: ", [Score]),
  emit_moves_to_stdout(lists:reverse(Moves)),
  {reply, Score, Score};
handle_call({result, _, Score}, _From, State) -> 
  %lager:info("Dropping score of ~w, because previous ~w was better.~n", [Score, State]),
  {reply, State, State}.
  
handle_cast({result, Moves, Score}, MaxScore) when Score > MaxScore ->
  lager:info("New score of ~w emitted, old score was ~w, new history is ~w~n", [Score, MaxScore, Moves]),
  emit_moves_to_stdout(lists:reverse(Moves)),
  {noreply, Score};
handle_cast({result, _, Score}, State) -> 
  lager:info("Dropping score of ~w, because previous ~w was better.~n", [Score, State]),
  {noreply, State}.
% handle_cast({add_job, Job}, {MaxScore, JobQueue}) -> 
%   % io:format("Added job: ~w~n", [Job]),
%   {noreply, {MaxScore, heaps:add(JobQueue, Job)}}.

handle_info(_, State) -> {noreply, State}.

emit_moves_to_stdout(Moves) ->
  io:format("[", []),
  emit_them_entirely(Moves),
  io:format("]~n", []).

emit_them_entirely([]) ->
  io:format("", []);
emit_them_entirely([{{X, Y}, _}|[]]) ->
  io:format("(~w,~w)", [X, Y]);
emit_them_entirely([{{X, Y}, _}|Moves]) ->
  io:format("(~w,~w),", [X, Y]),
  emit_them_entirely(Moves).
