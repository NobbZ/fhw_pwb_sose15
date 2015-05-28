-module(erlking_proxy).

-behaviour(gen_fsm).

-include("job.hrl").

%%% gen_fsm
-export([init/1, collecting/2, dropping/2]).

%%% api
-export([start_link/0,ask_for_job/1]).

-export_types([]).

-record(state, {worker = undefined                :: pid() | undefined,
                heap   = heaps:new(fun compare/2) :: heaps:t()}).

-define(MAX_HEAP_SIZE, 500000).
-define(MIN_HEAP_SIZE, (round(?MAX_HEAP_SIZE * 0.9))).

-define(MAX_MEM_USE, 1024 * 1024 * 1024 * 3).
-define(MIN_MEM_USE, (round(?MAX_MEM_USE * 0.9))).

ask_for_job(Proxy) ->
    gen_fsm:send_event(Proxy, ask_for_job).

compare(#job{potential = P1, lastscore = LS1}, #job{potential = P2, lastscore = LS2}) ->
    Pot1 = P1*P1+LS1,
    Pot2 = P2*P2+LS2,
    if Pot1 > Pot2 -> lt;
       true        -> gt
    end.

start_link() ->
    lager:info("start_link of proxy ~w", [self()]),
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    lager:info("init of proxy ~w", [self()]),
    Worker = spawn_link(erlking_worker2, start_link, [self()]),
    lager:info("proxy ~w has started worker ~w", [self(), Worker]),
    State  = #state{worker = Worker, heap = heaps:new(fun compare/2)},
    lager:info("proxy ~w enters state of collecting with state ~p",
               [self(), State]),
    {ok, collecting, State}.

collecting(ask_for_job, #state{worker = Worker, heap = Heap} = State) ->
    HeapSize = heaps:size(Heap),
    Heap2 = if HeapSize > 0 ->
                    {Job, Heap1} = heaps:fetch(Heap),
                    Worker ! Job,
                    Heap1;
               true -> Heap
            end,
    {next_state, collecting, State#state{heap = Heap2}};
collecting({add_job, Job}, #state{heap = Heap} = State) ->
    Heap1 = heaps:add(Heap, Job),
    %% NextState = case heaps:size(Heap1) >= ?MAX_HEAP_SIZE of
    %%                 true  -> dropping;
    %%                 false -> collecting
    %%             end,
    NextState = case erlang:memory(processes_used) >= ?MAX_MEM_USE of
                    true  -> lager:info("switched to dropping"), dropping;
                    false -> collecting
                end,
    {next_state, NextState, State#state{heap = Heap1}}.

dropping(ask_for_job, #state{worker = Worker, heap = Heap} = State) ->
    {Job, Heap1} = heaps:fetch(Heap),
    Worker ! Job,
    %% NextState = case heaps:size(Heap1) =< ?MIN_HEAP_SIZE of
    %%                 true  -> collecting;
    %%                 false -> dropping
    %%             end,
    NextState = case erlang:memory(processes_used) =< ?MIN_MEM_USE of
                    true  -> lager:info("switched to collecting"), collecting;
                    false -> dropping
                end,
    {next_state, NextState, State#state{heap = Heap1}};
dropping({add_job, Job}, #state{heap = Heap} = State) ->
    Heap1 = case compare(Job, heaps:peek(Heap)) of
                lt -> heaps:add(Heap, Job);
                eq -> heaps:add(Heap, Job);
                gt -> Heap
            end,
    %% NextState = case heaps:size(Heap1) =< ?MIN_HEAP_SIZE of
    %%                 true  -> collecting;
    %%                 false -> dropping
    %%             end,
    NextState = case erlang:memory(processes_used) =< ?MIN_MEM_USE of
                    true  -> lager:info("switched to collecting"), collecting;
                    false -> dropping
                end,
    {next_state, NextState, State#state{heap = Heap1}}.
