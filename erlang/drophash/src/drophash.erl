%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(drophash).

-include_lib("wx/include/wx.hrl").

%% API
-export([main/0, start_link/0]).

-spec main() -> any().
main() ->
    application:load(drophash),
    case drophash_win:start_link() of
        {error, _} = Error ->io:format("Error~n~p~n", [Error]);
        Window -> drophash_win:run(Window)
    end.

-spec start_link() -> {'ok', pid(), wx:wx_object()}.
start_link() ->
    case drophash_win:start_link() of
       {error, _} = E -> E;
        Window -> {ok, wx_object:get_pid(Window), Window}
    end.