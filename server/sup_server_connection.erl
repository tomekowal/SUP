
-module(sup_server_connection).

-export([start/2, server/1]).

%%------------------------------------------------------------------------------
%% @doc Start connection server.
%%
%% Creates process which spawns Num workers accepting connections.
%% @end
%%------------------------------------------------------------------------------
start(Num,LPort) ->
    %% {active, false} says that packets will NOT be sent to process mailbox,
    %% instead you have to receive them with gen_tcp:recv/2
    %% {packet, 2} creates 2 byte long header with length of data for every
    %% packet
    case gen_tcp:listen(LPort,[{active, false},{packet,2}]) of
        {ok, ListenSock} ->
            start_servers(Num,ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.

%%------------------------------------------------------------------------------
%% Spawns Num prcesses listening on LS
%%------------------------------------------------------------------------------
start_servers(0,_) ->
    ok;
start_servers(Num,LS) ->
    spawn_link(?MODULE,server,[LS]),
    start_servers(Num-1,LS).

%%------------------------------------------------------------------------------
%% Accept connection and start loop with returned socket S
%%------------------------------------------------------------------------------
server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.

%%------------------------------------------------------------------------------
%% Receive data and answer until socket is closed by client
%%------------------------------------------------------------------------------
loop(S) ->
    %% all incoming packets go to process message box,
    %% no need to use gen_tcp:recv()
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Data} ->
            Answer = process(Data), % Not implemented in this example
            gen_tcp:send(S,Answer),
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.

%%------------------------------------------------------------------------------
%% Buisness logic
%%------------------------------------------------------------------------------
process(Data) ->
    io:format("~p ~n", [Data]),
    %% here should go functions which output
    %% {Request, Body} for client
    Request = get_data,
    Body = [body, with, list, inside],
    mochijson2:encode({struct, [{Request, Body}]}).
