
-module(sup_server_connection).

-export([start/0, server/1]).

%%------------------------------------------------------------------------------
%% @doc Start connection server.
%%
%% Creates process which spawns Num workers accepting connections.
%% @end
%%------------------------------------------------------------------------------
start() ->
    %% {active, false} says that packets will NOT be sent to process mailbox,
    %% instead you have to receive them with gen_tcp:recv/2
    %% {packet, 2} creates 2 byte long header with length of data for every
    %% packet
    ListenPort = 45894,
    Num = 10,
    case gen_tcp:listen(ListenPort, [{active, false},{packet,2}]) of
        {ok, ListenSocket} ->
            start_servers(Num, ListenSocket),
            {ok, Port} = inet:port(ListenSocket),
            Port;
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% Spawns Num prcesses listening on ListenSocket
%%------------------------------------------------------------------------------
start_servers(0, _) ->
    ok;
start_servers(Num, ListenSocket) ->
    spawn_link(?MODULE, server, [ListenSocket]),
    start_servers(Num-1, ListenSocket).

%%------------------------------------------------------------------------------
%% Accept connection and start loop with returned Socket
%%------------------------------------------------------------------------------
server(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            loop(Socket),
            server(ListenSocket);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

%%------------------------------------------------------------------------------
%% Receive data and answer until socket is closed by client
%%------------------------------------------------------------------------------
loop(Socket) ->
    %% all incoming packets go to process message box,
    %% no need to use gen_tcp:recv()
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            Answer = process(Data),
            gen_tcp:send(Socket, Answer),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed [~w]~n", [Socket, self()]),
            ok
    end.

%%------------------------------------------------------------------------------
%% Business logic
%%------------------------------------------------------------------------------
process(Data) ->
    io:format("~p ~n", [Data]),
    %% here should go functions which output
    %% {Request, Body} for client
    Request = get_data,
    Body = [body, with, list, inside],
    mochijson2:encode({struct, [{Request, Body}]}).
