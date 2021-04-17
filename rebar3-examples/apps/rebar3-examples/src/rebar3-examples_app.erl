%%%-------------------------------------------------------------------
%% @doc rebar3-examples public API
%% @end
%%%-------------------------------------------------------------------

-module(rebar3-examples_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rebar3-examples_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
