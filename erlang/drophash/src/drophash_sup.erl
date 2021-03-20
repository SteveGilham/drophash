%%%-------------------------------------------------------------------
%% @doc drophash top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(drophash_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', _}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> {'ok', {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpec = #{
        id      => drophash, % anything but a pid()
        start   => {drophash, start_link, []}
    },                 
    ChildSpecs = [ ChildSpec ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
