%%%-------------------------------------------------------------------
%%% @author Norbert Melzer
%%% @copyright (C) 2015, Norbert Melzer
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2015 14:09
%%%-------------------------------------------------------------------
-author("Norbert Melzer").

-record(individual, {max_hit_count = nil,
                     g = nil,
                     g_min = nil,
                     p = nil,
                     f = 0,
                     clears = false,
                     max_hit_idx = nil}).
