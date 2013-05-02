%%%-------------------------------------------------------------------
%%% File    : irc_connection.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(irc_connection).

%% API
-export([start/3, send/2, stop/1]).

-include("irc.hrl").

-record(state, {socket, pid, buffer = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: connect(Host, Port)
%% Description:
%%--------------------------------------------------------------------
start(Host, Port, ReportPid) ->
    spawn_link(fun() ->
                       {ok, Sock} = connect(Host, Port),
                       ReportPid ! socket_opened, 
                       loop(#state{socket=Sock, pid = ReportPid})        
               end).

send(Pid, Cmd) ->
    Pid ! {send, Cmd}.

stop(Pid) ->
    Pid ! stop.

%%====================================================================
%% Internal functions
%%====================================================================
connect(Host, Port) ->
    Opts = [list,
            {nodelay, true},
            {keepalive, true},
            {reuseaddr, true},
            {active, true}],

    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Sock} ->           
            {ok, Sock};
        {error, _Reason} ->
            timer:sleep(30000),
            connect(Host, Port)
    end.

loop(State) ->
    receive
        {send, Msg} -> % See irc_parser:encode_message/1 for format of Msg
            Data = irc_parser:encode_message(Msg),
            io:format("> ~s", [Data]),
            gen_tcp:send(State#state.socket, Data),
            loop(State);

        {tcp, _Port, Data} ->
            RawData = lists:concat([State#state.buffer, Data]),
            Parsed = irc_parser:parse_data(RawData),

            Unparsed = 
                [if is_record(Cmd, irc_cmd) ->
                         io:format("< ~s~n", [Cmd#irc_cmd.raw]),
                         case Cmd#irc_cmd.name of
                             ping ->
                                 %% io:format("~p: PING? PONG!~n", [?MODULE]),
                                 self() ! {send, {pong, Cmd#irc_cmd.args}};
                             _ ->
                                 State#state.pid ! {received, Cmd}
                         end,
                         "";
                    %% save unparsed line for next round
                    is_list(Cmd) -> Cmd
                 end
                 || Cmd <- Parsed],
            loop(State#state{buffer = lists:flatten(Unparsed)});

        {tcp_closed, _Port} ->
            io:format("~p: Connection closed~n", [?MODULE]),
            State#state.pid ! socket_closed,
            ok;

        stop ->
            io:format("~p: Terminating~n", [?MODULE]),
            gen_tcp:close(State#state.socket),
            ok;

        Other ->
            io:format("~p: Unknown message ~p~n", [?MODULE, Other]),
            loop(State)
    end.
