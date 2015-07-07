%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 08. Jun 2015 08:24
%%%-------------------------------------------------------------------
-module(ek_result).
-author("Norbert Melzer").

-behaviour(gen_server).

-record(state, {score = -65535 :: integer()}).

%% gen_server
-export([init/1, terminate/2, handle_info/2, handle_cast/2, handle_call/3,
         code_change/3]).

%% API
-export([start_link/0, report/2, get_score/0]).

%%%-----------------------------------------------------------------------------
%%% gen_server
%%%-----------------------------------------------------------------------------

init([]) ->
  {ok, #state{}}.

terminate(Reason, State) ->
  lager:info("~p shuts down because ~p, last state was ~p.",
             [?MODULE, Reason, State]).

handle_info(Info, State) ->
  lager:info(
    "~p received ~p during state ~p, I don't know what to do and STOP!",
    [?MODULE, Info, State]),
  {stop, unknown_signal, State}.

handle_cast(Request, State) ->
  lager:info(
    "~p received ~p as cast during state ~p, I don't know what to do and STOP!",
    [?MODULE, Request, State]),
  {stop, unknown_cast, State}.

handle_call({report, Score, History}, _, #state{} = S) ->
  Reply = case Score > S#state.score of
    true ->
      %% io:format("Score: ~p: ", [Score]),
      print_history(History),
      Score;
    false -> S#state.score
  end,
  {reply, Reply, S#state{score = Reply}};
handle_call(get_score, _, #state{} = S) ->
  {reply, S#state.score, S};
handle_call(Request, From, State) ->
  lager:info(
    "~p received ~p as call during state ~p from ~p, I don't know what to do and STOP!",
    [?MODULE, Request, State, From]),
  {stop, unknown_call, State}.

code_change(_, State, _) ->
  {ok, State}.

%%%-----------------------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

report(Score, History) ->
  gen_server:call(?MODULE, {report, Score, History}).

get_score() ->
  gen_server:call(?MODULE, get_score).

%%%-----------------------------------------------------------------------------
%%% private
%%%-----------------------------------------------------------------------------

print_history(History) ->
  really_print_history(lists:reverse(History)).

really_print_history(History) ->
  HistoryString = format_history(History),
  io:format("[~s]~n", [HistoryString]).

format_history([]) ->
  "";
format_history([{{X, Y}, _}]) ->
  io_lib:format("(~p, ~p)", [X, Y]);
format_history([{{X, Y}, _}|Tail]) ->
  lists:concat([io_lib:format("(~p, ~p), ", [X, Y]), format_history(Tail)]).
