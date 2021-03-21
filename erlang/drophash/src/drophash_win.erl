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
    text         :: wxStyledTextCtrl:wxStyledTextCtrl()
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
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wxErlang - Drophash", []),
    Text = wxStyledTextCtrl:new(Frame,
         [{style, ?wxTE_MULTILINE bor ?wxTE_DONTWRAP bor ?wxTE_READONLY} ]),
    wxStyledTextCtrl:addText(Text, "Drop files to hash"),        
    wxStyledTextCtrl:dragAcceptFiles(Text, true),
    wxStyledTextCtrl:connect(Text, drop_files),

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

handle_event(#wx{event = #wxDropFiles{type = drop_files} = Event}, S) ->
    Before = wxStyledTextCtrl:getText(S#state.text),
    case Before of
      "Drop files to hash" -> wxStyledTextCtrl:clearAll(S#state.text);
      _ -> noop
    end,
    Write = fun (F) -> write_file(F, S#state.text) end,
    Files = Event#wxDropFiles.files,
    lists:foreach(Write, Files),
    {noreply, S};
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
    
%% Internal %%
write_file(File, Text) ->
  wxStyledTextCtrl:appendText(Text, [File, 10]),
  IO = file:read_file(File),
  case IO of
  {ok, Data} ->
    Hash = fun (A) -> hash_file(A, Data) end,
    Hashes = lists:map(Hash, [ md5, sha, sha256 ]),
    lists:foreach(fun (H) ->
      wxStyledTextCtrl:appendText(Text, [H, 10])
      end, Hashes),
    wxStyledTextCtrl:appendText(Text, [10]);
  _ -> io:format("hash_file Failure:~n~p~n~p~n", [IO, File])
  end.
    

hash_file(Algorithm, Data) ->
  Hash = crypto:hash(Algorithm, Data),
  Bytes = binary:bin_to_list(Hash),
  Hexer = fun (B) -> byte_to_hex(B) end,
  Hexed = lists:map(Hexer, Bytes),
  Chunked = chunk(Hexed),
  Type = atom_to_binary(Algorithm),
  [string:pad(Type, 6), ": ", Chunked].
  
chunk([]) -> [];
chunk(L) ->
  %% list is a list of lists, each inner list is a byte as 2 nybbles
  {Head, Tail} = lists:split(2, L),
  NewTail = chunk(Tail),
  [ Head | [ " " | NewTail]].
  
byte_to_hex(B)  when B < 256 ->
    [nybble_to_hex(B div 16), nybble_to_hex(B rem 16)].
  
nybble_to_hex(N) when N < 10 ->
    $0+N;
nybble_to_hex(N)  when N >= 10, N < 16 ->
    $a+(N-10).
  
