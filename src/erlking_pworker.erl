-module(erlking_pworker).

-behaviour(gen_server).

-export([init/1, handle_cast/2]).

-export([start_link/0]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  lager:info("init of pworker ~w", [self()]),
  {ok, ProxyPid} = erlking_proxy:start_link(),
  {ok, ProxyPid}.

handle_cast(Msg, ProxyPid) ->
  %lager:info("pworker received message '~p'", [Msg]),
  gen_fsm:send_event(ProxyPid, Msg),
  {noreply, ProxyPid}.
