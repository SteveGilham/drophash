%%%-------------------------------------------------------------------
%% @doc drophash top level window.
%% @end
%%%-------------------------------------------------------------------
-module(drophash_win).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

%% API
-export_type([drophash/0]).
-export([start/0, start_link/0, stop/1, run/1]).


%% wx_object callbacks
-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    frame      :: wxFrame:wxFrame(),
    text         :: wxTextCtrl:wxTextCtrl()
}).

-type    state() :: #state{}.
-type drophash() :: wxWindow:wxWindow().

-spec start() -> drophash() | {'error', any()}.
start()         -> wx_object:start(?MODULE, [], []).

-spec start_link() -> drophash() | {'error', any()}.
start_link()         -> wx_object:start_link(?MODULE, [], []).

-spec stop(drophash()) -> 'ok'.
stop(Drophash) -> wx_object:stop(Drophash).

-spec run(drophash()) -> 'ok'.
run(Drophash) -> catch wx_object:call(Drophash, noreply), ok.

%% object_wx callbacks
-spec init(list()) -> {wxFrame:wxFrame(), state()}.
init(_) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "drophash", []),
    Text = wxTextCtrl:new(Frame, ?wxID_ANY),
    wxTextCtrl:setEditable(Text, false),
    %% verify fill %% wxWindow:setBackgroundColour(Text, {0,0,127}),
    
    wxFrame:show(Frame),
    wxFrame:raise(Frame),

    {Frame,
        #state{
            frame = Frame,
            text = Text
        }
    }.

-spec handle_event(Event :: wx(), State :: state())
        -> {'noreply', state()}
         | {'stop', 'normal', state()}.

handle_event(Event, S) ->
    io:format("Unhandled Event:~n~p~n", [Event]),
    {noreply, S}.

-spec handle_call(Request::any(), From::any(), State::state())
        -> {'noreply', state()} | {reply, ok, state()}.
handle_call(noreply, _From, State) ->
    {noreply, State}; % wait until window closed

handle_call(Request, _From, State) ->
    io:format("Unhandled Call:~n~p~n", [Request]),
    {reply, ok, State}.

-spec handle_cast(Request::any(), State::state()) -> {'noreply', state()}.

handle_cast(Request, State) ->
    io:format("Unhandled Cast:~n~p~n", [Request]),
    {noreply, State}.

-spec handle_info(Info::any(), State::state()) -> {'noreply', state()}.
handle_info(Info, State) ->
    io:format("Unhandled Info:~n~p~n", [Info]),
    {noreply, State}.

-spec terminate(Reason::any(), State::state()) -> 'ok'.
terminate(_Reason, S) ->
    wxFrame:destroy(S#state.frame),
    wx:destroy(),
    ok.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.