-module(ek_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> Result when
  Result :: {ok, pid()}
  | ignore
  | {error, Reason},
  Reason :: {already_started, pid()}
  | {shutdown, term()}
  | term().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(Args) -> Result when
  Args :: [],
  Result :: {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}
  | ignore,
  RestartStrategy :: supervisor:strategy(),
  MaxR :: non_neg_integer(),
  MaxT :: pos_integer(),
  ChildSpec :: supervisor:child_spec().
init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    queue()
  ]}}.

queue() ->
  {ek_queue, {ek_queue, start_link, []}, permanent, 5000, worker, [ek_queue]}.

