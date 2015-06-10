%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 03. Jun 2015 23:38
%%%-------------------------------------------------------------------
-module(ek_individual).
-author("Norbert Melzer").

%% API
-export([new/1, new/2, compare/2, equals/2, mutate/1, one_point_crossover/2,
         one_point_crossover/3, set_output_attributes/2]).

-include("individual.hrl").

new(MHC) ->
  Genotype = lists:map(fun(_) -> random:uniform(MHC) end, lists:seq(1, MHC)),
  new(MHC, Genotype).

new(MHC, Genotype) ->
  #individual{max_hit_count = MHC,
              g             = Genotype,
              max_hit_idx   = MHC}.

compare(#individual{max_hit_idx = MHI1, g = G1},
        #individual{max_hit_idx = MHI2, g = G2}) ->
  CompIdx = max(MHI1, MHI2),
  ek_genome:compare(G1, G2, CompIdx).

equals(#individual{max_hit_idx = MHI1, g = G1},
       #individual{max_hit_idx = MHI2, g = G2}) ->
  CompIdx = max(MHI1, MHI2),
  ek_genome:equals(G1, G2, CompIdx).

mutate(#individual{max_hit_count = MHC} = I) ->
  GList = lists:seq(1, MHC),
  NewG = lists:map(fun(_) -> random:uniform(MHC) end, GList),
  I#individual{clears      = false,
               max_hit_idx = MHC,
               g           = NewG}.

one_point_crossover(#individual{} = I1,
                    #individual{} = I2) ->
  one_point_crossover(I1, I2, 0).

one_point_crossover(#individual{max_hit_count = MHC1, g = G1} = I1,
                    #individual{g = G2},
                    Idx) ->
  NewG = ek_genome:splice(G1, G2, Idx),
  I1#individual{clears      = false,
                max_hit_idx = MHC1,
                g           = NewG}.

set_output_attributes(#individual{} = I, SameGame) ->
  samegame:phenotype_of(SameGame, I).

