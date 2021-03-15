%%%-------------------------------------------------------------------
%% @doc drophash public API
%% @end
%%%-------------------------------------------------------------------

-module(drophash_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    drophash_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
