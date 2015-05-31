-module(ek_queue).
-behaviour(gen_server).

% API
-export([start_link/0, result/2]).

% Exports for implementing behaviour 'gen_server'.
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
  terminate/2]).

-include("job.hrl").

-define(return(Score), {reply, Score, Score}).

-spec start_link() -> Result when
  Result :: {ok, Pid}
  | ignore
  | {error, Error},
  Pid :: pid(),
  Error :: {already_started, Pid}
  | term().
start_link() ->
  lager:info("Starting ~p", [?MODULE]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec result([term()], non_neg_integer()) -> non_neg_integer().
result(History, Score) ->
  gen_server:call(?MODULE, {result, History, Score}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(term()) -> {ok, 0, infinity}.
init(_) ->
  %% erlking_pool:add_job(read_board),
  {ok, -4294836225, infinity}.

-spec code_change(term(), term(), term()) -> {error, term()}.
code_change(_, _, _) ->
  {error, "Hot updating not supported."}.

-spec handle_call({result, term(), non_neg_integer()}, pid(), non_neg_integer()) ->
  {reply, non_neg_integer(), non_neg_integer()}.
handle_call({result, History, Score}, _From, MaxScore) when Score > MaxScore ->
  lager:info("New Score: ~p", [Score]),
  emit_history(lists:reverse(History)),
  ?return(Score);
handle_call(_, _From, State) ->
  ?return(State).

-spec handle_cast(term(), non_neg_integer()) -> {noreply, non_neg_integer()}.
handle_cast(_, State) ->
  {noreply, State}.

-spec handle_info(term(), non_neg_integer()) -> {noreply, non_neg_integer()}.
handle_info(_, State) -> {noreply, State}.

-spec terminate(term(), term()) -> ok.
terminate(_, _) ->
  ok.

emit_history(History) ->
  MoveString = move_string2(History),
  io:format("[~s]~n", [MoveString]).

move_string2([]) ->
  "";
move_string2([{X, Y}]) ->
  io_lib:format("(~p, ~p)", [X, Y]);
move_string2([{X, Y} | Tail]) ->
  lists:concat([io_lib:format("(~p, ~p), ", [X, Y]), move_string2(Tail)]).
