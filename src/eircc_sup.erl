
-module(eircc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_client/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Options) ->
    supervisor:start_child(
      ?MODULE, 
      [proplists:get_value(Key, Options, Def) 
       || {Key, Def} <-
              [{host, "localhost"},
               {port, 7000},
               {nick, "eircc"},
               {pass, undefined},
               {name, "Erlang IRC Client"}
              ]
      ]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, 
           [?CHILD(irc_client, worker)]} }.

