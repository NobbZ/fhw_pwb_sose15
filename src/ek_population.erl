%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2015 12:56
%%%-------------------------------------------------------------------
-module(ek_population).
-author("Norbert Melzer").

%% API
-export([new/2, new/3, remove_unfinished/1, best_individual/1, merge/2,
         calculate_fitness/2, next_generation/1]).

-record(population, {individuals = [],
                     population = 0}).

-include("individual.hrl").

-define(POPULATION, 100).
-define(GEN_IDX, (ceil(?POPULATION / 10))).
-define(MUTATE_PROP, 0.1).
-define(RECOMBINE_PROP, 0.1).

new(Cols, Rows) ->
  new(?POPULATION, Cols, Rows).

new(PopCount, Cols, Rows) ->
  F = fun(_) -> ek_individual:new(Cols * Rows / 2) end,
  #population{population  = PopCount,
              individuals = lists:map(F, lists:seq(1, PopCount))}.

comp_f(I1, I2) ->
  if
    I1#individual.f < I2#individual.f -> lt;
    I1#individual.f == I2#individual.f -> eq;
    true -> gt
  end.

remove_unfinished(#population{individuals = Is} = Pop) ->
  NewIs = lists:filter(fun(I) -> not I#individual.clears end, Is),
  SortedIs = heaps:sort(NewIs, fun comp_f/2),
  Pop#population{individuals = SortedIs,
                 population  = length(SortedIs)}.

best_individual(#population{individuals = Is}) ->
  Heap = heaps:from_list(Is, fun comp_f/2),
  heaps:peek(Heap).

calculate_fitness(#population{} = Pop, SameGame) ->
  F = fun(I) -> calculate_fitness(I, SameGame) end,
  NewIs = lists:map(F, Pop#population.individuals),
  Pop#population{individuals = NewIs};
calculate_fitness(#individual{} = I, SameGame) ->
  ek_samegame:fitness_of(SameGame, I).

next_generation(#population{} = Pop) ->
  RecombinedPop = recombine(Pop),
  mutate(RecombinedPop).

recombine(#population{} = Pop) ->
  F = fun({L, R}) ->
    case ?RECOMBINE_PROP >= 1.0 orelse random:uniform() =< ?RECOMBINE_PROP of
      true ->
        ek_individual:one_point_crossover(L, R, random:uniform(
          L#individual.max_hit_idx)             div 2);
      false -> L
    end
  end,
  RightParts = shuffle_list(Pop#population.individuals),
  Pairs = lists:zip(Pop#population.individuals, RightParts),
  CrossBreeds = lists:map(F, Pairs),
  Pop#population{population = CrossBreeds}.


mutate(#population{} = Pop) ->
  F = fun(I) ->
    case ?MUTATE_PROP >= 1.0 orelse random:uniform() =< ?MUTATE_PROP of
      true ->
        ek_individual:mutate(I);
      false -> I
    end
  end,
  MutatedIs = lists:map(F, Pop#population.individuals),
  Pop#population{individuals = MutatedIs}.

merge_p(#population{} = Self, #population{} = Other) ->
  F = fun(#individual{max_hit_idx = X}, Min) ->
    case Min of
      nil -> X;
      _ when X > Min -> Min;
      _ -> X
    end
  end,
  MinMHIdxPar = lists:foldr(F, nil, Self#population.individuals),
  Parents =
  lists:reverse(heaps:sort(Self#population.individuals, fun comp_f/2)),
  UniqueParents = filter_doubles_on_f(Parents, MinMHIdxPar),
  MinMHIdxChild = lists:foldr(F, nil, Other),
  Children =
  lists:reverse(heaps:sort(Other#population.individuals, fun comp_f/2)),
  UniqueChildren = filter_doubles_on_f(Children, MinMHIdxChild),
  MergedPop = ek_list_helpers:take(UniqueParents, ?GEN_IDX) ++
              ek_list_helpers:take(UniqueChildren, ?GEN_IDX),
  %%RestCount = (?POPULATION - ?GEN_IDX) * 2,
  %%AddParents = ek_list_helpers:take(ek_list_helpers:drop(UniqueChildren, ?GEN_IDX), ?POPULATION - ?GEN_IDX - 1),
  %%AddChildren = ek_list_helpers:take(ek_list_helpers:drop(UniqueChildren, ?GEN_IDX), ?POPULATION - ?GEN_IDX - 1),
  %%EnhancedPop = MergedPop ++ AddParents ++ AddChildren.
  Heap = heaps:from_list(MergedPop, fun comp_f/2),
  NewSize = heaps:size(Heap),
  Return = lists:reverse(heaps:to_list(Heap)),
  {Return, NewSize}.

merge(#population{} = Self, #population{} = Other) ->
  {NewInd, NewPopSize} = merge_p(Self, Other),
  #population{population  = length(NewPopSize),
              individuals = NewInd}.

filter_doubles_on_f([], _)  -> [];
filter_doubles_on_f([L], _) -> [L];
filter_doubles_on_f([#individual{g = L}, #individual{g = R}|T], MHI) ->
  case ek_genome:equals(L, R, MHI) of
    true -> filter_doubles_on_f([L|T], MHI);
    false -> [L|filter_doubles_on_f([R|T], MHI)]
  end.

shuffle_list(List) ->
  [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- List])].

ceil(X) when X < 0 -> trunc(X);
ceil(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.
