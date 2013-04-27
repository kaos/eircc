%%%-------------------------------------------------------------------
%%% File    : irc_bot.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(irc_client).
-behaviour(gen_fsm).

%% API
-export([start_link/1,
         init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3,
         stop/1,
         send_message/2
        ]).

%% States
-export([connecting/2,
         authenticating/2,
         joining/2,
         connected/2]).

-include("irc.hrl").
-include("eircc.hrl").

-record(state, {connection, hostname, port, channel,
                nickname, password, real_name, owner}).

%%====================================================================
%% API
%%====================================================================

start_link([Hostname, Port, Channel, Nickname, Password, RealName, Owner]) ->
    gen_fsm:start_link(?MODULE, #state{
                                   hostname=Hostname,
                                   port=Port,
                                   channel=Channel,
                                   nickname=Nickname,
                                   password=Password,
                                   real_name=RealName,
                                   owner=Owner
                                  }, []).

stop(Ref) ->
    gen_fsm:send_all_state_event(Ref, stop).

send_message(Ref, Message) ->
    gen_fsm:send_event(Ref, {send_message, Message}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> Result
%% Description:
%%--------------------------------------------------------------------
init(State) ->    
    Connection = irc_connection:start(State#state.hostname,
                                      State#state.port,
                                      self()),
    {ok, connecting, State#state{connection=Connection}}.

connecting(socket_opened, State) ->
    io:format("~p: Connected, sending login information~n", [?MODULE]),

    Connection = State#state.connection,
    Nickname = State#state.nickname,
    RealName = State#state.real_name,

    irc_connection:send(Connection, {nick, Nickname}),
    irc_connection:send(Connection, {user, Nickname, "0", "*", RealName}),

    {next_state, authenticating, State};
connecting({send_message, _}, State) ->
    %% drop
    {next_state, connecting, State}.

authenticating({authenticated, _}, State) ->
    Connection = State#state.connection,
    Channel = State#state.channel,
    Password = State#state.password,

    irc_connection:send(Connection, {nickserv_identify, Password}),
    io:format("~p: Joining channel ~p~n", [?MODULE, Channel]),
    irc_connection:send(Connection, {join, Channel}),
    {next_state, joining, State};
authenticating({send_message, _}, State) ->
    %% drop
    {next_state, authenticating, State}.

joining({joined, _}, State) ->
    %%Connection = State#state.connection,
    %%Channel = State#state.channel,

    io:format("~p: Joined channel~n", [?MODULE]),
    %%irc_connection:send(Connection, {say, Channel, "Hello!"}),
    {next_state, connected, State};
joining({send_message, _}, State) ->
    %% drop
    {next_state, joining, State}.

connected({message, Command}, State) ->
    {Recipient, Message} = split_message_arg(Command#irc_cmd.args),
    Sender = (Command#irc_cmd.source)#user.nick,
    Nick = State#state.nickname,
    case Recipient of
        Nick ->
            io:format("~p: Private message from ~s: ~s~n",
                      [?MODULE, Sender, Message]),
            State#state.owner ! #eircc{ from=Sender, message=Message };
        Channel ->
            io:format("~p: Message to channel ~s from ~s: ~s~n",
                      [?MODULE, Recipient, Sender, Message]),
            State#state.owner ! #eircc{ from=Sender, channel=Channel, message=Message}
    end,

    {next_state, connected, State};
connected({send_message, Message}, 
          #state{ connection=Con, channel=Chan }=State) ->
    Cmd = case Message of
              "/me " ++ Action ->
                  {action, Chan, Action};
              _ -> 
                  {say, Chan, Message}
          end,
    irc_connection:send(Con, Cmd),
    {next_state, connected, State}.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(stop, _StateName, State) ->    
    {stop, normal, State};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(socket_opened, StateName, StateData) ->
    gen_fsm:send_event(self(), socket_opened),
    {next_state, StateName, StateData};

handle_info({received, Command}, StateName, StateData) ->   
    case process_command(StateName, Command#irc_cmd.name) of
        undefined ->
            ok;
        Event ->
            gen_fsm:send_event(self(), {Event, Command})
    end,
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, _StateName, StateData) ->
    io:format("~p: Terminating (~p)~n", [?MODULE, Reason]),
    irc_connection:terminate(StateData#state.connection),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
process_command(authenticating, endofmotd) -> authenticated;
process_command(joining, join)             -> joined;
process_command(connected, privmsg)        -> message;
process_command(_, _) -> undefined.

split_message_arg(Data) ->
    Index = string:str(Data, " :"),
    {string:substr(Data, 1, Index-1),
     string:substr(Data, Index+2, string:len(Data) - (Index-1))}.
