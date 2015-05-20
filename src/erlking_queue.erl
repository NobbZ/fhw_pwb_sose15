-module (erlking_queue).
-behaviour (gen_server).

% API
-export ([start_link/0, result/2]).

% Exports for implementing behaviour 'gen_server'.
-export ([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2]).

-spec start_link() -> Result when
      Result :: {ok, Pid}
              | ignore
              | {error, Error},
      Pid    :: pid(),
      Error  :: {already_started, Pid}
              | term().
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec result([term()], non_neg_integer()) -> non_neg_integer().
result(Moves, Score) ->
  gen_server:call(?MODULE, {result, Moves, Score}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(term()) -> {ok, 0, infinity}.
init(_) ->
  erlking_pool:add_job(read_board),
  {ok, 0, infinity}.

-spec code_change(term(), term(), term()) -> {error, term()}.
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

-spec handle_call({result, term(), non_neg_integer()}, pid(), non_neg_integer()) ->
                         {reply, non_neg_integer(), non_neg_integer()}.
handle_call({result, Moves, Score}, _From, MaxScore) when Score > MaxScore ->
  %lager:info("New score of ~w emitted, old score was ~w, new history is ~w~n", [Score, MaxScore, Moves]),
  lager:info("New Score: ~w: ", [Score]),
  emit_moves_to_stdout(lists:reverse(Moves)),
  {reply, Score, Score};
handle_call({result, _, _}, _From, State) -> 
  {reply, State, State}.

-spec handle_cast(term(), non_neg_integer()) -> {noreply, non_neg_integer()}.
handle_cast(_, State) ->
    {noreply, State}.
% handle_cast({add_job, Job}, {MaxScore, JobQueue}) -> 
%   % io:format("Added job: ~w~n", [Job]),
%   {noreply, {MaxScore, heaps:add(JobQueue, Job)}}.

-spec handle_info(term(), non_neg_integer()) -> {noreply, non_neg_integer()}.
handle_info(_, State) -> {noreply, State}.

-spec terminate(term(), term()) -> ok.
terminate(_ ,_) ->
    ok.

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
