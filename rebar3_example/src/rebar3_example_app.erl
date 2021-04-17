%%%-------------------------------------------------------------------
%% @doc rebar3_example public API
%% @end
%%%-------------------------------------------------------------------

-module(rebar3_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    db_manager:start(),
    rebar3_example_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
