%%%-------------------------------------------------------------------
%% @doc drophash public API
%% @end
%%%-------------------------------------------------------------------

-module(drophash_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), _)
        -> {'ok',pid()} | {'ok',pid(),_} | {'error',_}.
start(_StartType, _StartArgs) ->
    drophash_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.

%% internal functions
