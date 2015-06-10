-module(erlking_proxy).

-behaviour(gen_fsm).

%%% gen_fsm
-export([init/1, collecting/2]).

%%% api
-export([start_link/0]).

-export_types([]).

-record(state, {worker = undefined                :: pid() | undefined,
                heap   = heaps:new(fun compare/2) :: heaps:t()}).

-define(MAX_HEAP_SIZE, 10000).
-define(MIN_HEAP_SIZE, (round(?MAX_HEAP_SIZE * 0.9))).

ask_for_job(Proxy) ->
    gen_fsm:send_event(Proxy, ask_for_job).

compare(foo, bar) ->
    undefined.

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @todo spawn real worker
init([]) ->
    Worker = undefined, %spawn_link(erlking_worker2, start_link, [self()]),
    State  = #state{worker = Worker},
    {ok, collecting, State}.

collecting(ask_for_job, #state{worker = Worker, heap = Heap} = State) ->
    {Job, Heap1} = heaps:fetch(Heap),
    Worker ! Job,
    {next_state, collecting, State#state{heap = Heap1}};
collecting({add_job, Job}, #state{heap = Heap} = State) ->
    Heap1 = heaps:add(Heap, Job),
    NextState = case heaps:size(Heap1) >= ?MAX_HEAP_SIZE of
                    true  -> dropping;
                    false -> collecting
                end,
    {next_state, NextState, State#state{heap = Heap1}}.

dropping(ask_for_job, #state{worker = Worker, heap = Heap} = State) ->
    {Job, Heap1} = heaps:fetch(Heap),
    Worker ! Job,
    NextState = case heaps:size(Heap1) =< ?MIN_HEAP_SIZE of
                    true  -> collecting;
                    false -> dropping
                end,
        {next_state, NextState, State#state{heap = Heap1}};
dropping({add_job, Job}, #state{heap = Heap} = State) ->
    Heap1 = case compare(Job, heaps:peek(Heap)) of
                lt -> heaps:add(Heap, Job);
                eq -> heaps:add(Heap, Job);
                gt -> Heap
            end,
    NextState = case heaps:size(Heap1) =< ?MIN_HEAP_SIZE of
                    true  -> collecting;
                    false -> dropping
                end,
    {next_state, NextState, State#state{heap = Heap1}}.
