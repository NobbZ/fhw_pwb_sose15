%% @doc Mainmodule
%% @author Norbert Melzer <inf100760@fh-wedel.de>
-module (erlking).
-export ([main/1]).

%% @doc This does nothing but starting the app.
%% This function is needed because of <tt>rebar</tt>s <tt>escriptize</tt>
%% subcommand, which creates a wrapper-shell-script, which again calls this
%% function.
main(_Args) ->
  erlking_app:start([],[]).
