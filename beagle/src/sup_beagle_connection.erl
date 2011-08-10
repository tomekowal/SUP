
-module(sup_beagle_connection).

-export([start/0, init/0]).

-define(CONNECTION_REQUEST_INTERVAL, 5000).
-define(CONNECTION_TIMEOUT, 5000).

%%------------------------------------------------------------------------------
%% @doc Initialize loop for receiving requests
%% @end
%%------------------------------------------------------------------------------
start() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.
%%------------------------------------------------------------------------------
%% @doc Initialize loop for receiving requests
%% @end
%%------------------------------------------------------------------------------
init() ->
    Port = 5678,                            % can be in macro definition
    ServerHost = "localhost",               % can be in macro definition
    loop(ServerHost, Port).

%%------------------------------------------------------------------------------
%% Main loop periodically sends connection request.
%%
%% It can be forced to do it now when receives connection_request.
%% It is stopped by stop.
%%------------------------------------------------------------------------------
loop(ServerHost, Port) ->
    receive
        connection_request ->
            connection_request(ServerHost, Port),
            loop(ServerHost, Port);
        stop ->
            ok
    after
        ?CONNECTION_REQUEST_INTERVAL ->
            connection_request(ServerHost, Port),
            loop(ServerHost, Port)
    end.

%%------------------------------------------------------------------------------
%% Opens tcp socket and calls receive_reply()
%%------------------------------------------------------------------------------
connection_request(ServerHost, Port) ->
    case gen_tcp:connect(ServerHost, Port,
                                 [binary, {packet, 2}]) of
        {ok, Sock} ->
            ok = gen_tcp:send(Sock, "Connection Request"),
            receive_reply(),
            ok = gen_tcp:close(Sock);
        {error, Reason} ->
            io:format("~w~n", [Reason])
    end.

%%------------------------------------------------------------------------------
%% Waits for request from server and decodes it from json format.
%%------------------------------------------------------------------------------
receive_reply() ->
    receive
        stop ->
            ok;
        {tcp, _S, Binary} ->
            case mochijson2:decode(Binary) of
                {struct, [{Request, Body}]} ->
                    handle(Request, Body);
                Other ->
                    io:format("Something is wrong: ~p ~n", [Other])
            end;
        Other ->
            io:format("~p ~n", [Other])
    after
        ?CONNECTION_TIMEOUT ->
            return
    end.

%%------------------------------------------------------------------------------
%% Communication Protocol.
%%------------------------------------------------------------------------------
handle(<<"get_data">>, Body) ->
    io:format("get data ~p ~n", [Body]);
handle(_AnythingElse, _Body) ->
    io:format("what a terrible failure").


