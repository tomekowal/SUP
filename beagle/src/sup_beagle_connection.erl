-module(sup_beagle_connection).

-export([start/0, init/1]).

-define(CONNECTION_REQUEST_INTERVAL, 5000).
-define(CONNECTION_TIMEOUT, 5000).

%%------------------------------------------------------------------------------
%% @doc Initialize loop for receiving requests
%% @end
%%------------------------------------------------------------------------------
start() ->
    Pid = spawn_link(?MODULE, init, [loud]),
    Pid_q = spawn_link(?MODULE, init, [quiet]),
    {ok, Pid, Pid_q}.
%%------------------------------------------------------------------------------
%% @doc Initialize loop for receiving requests
%% @end
%%------------------------------------------------------------------------------
init(Mode) ->
	case Mode of
		quiet ->
			Port = 6789,
			case gen_tcp:listen(Port, [{active, false},{packet,2}]) of
        		{ok, Socket} ->
					quiet_loop(Socket);
				{error, Reason} ->
				    {error, Reason}
			end;
		loud ->
			{ok, ServerHost} = application:get_env(sup_beagle, management_host),
            {ok, Port} = application:get_env(sup_beagle, management_port),
			loop(ServerHost, Port)
	end.

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
%% Waits for connections.
%%------------------------------------------------------------------------------
quiet_loop(ListenSocket)->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            inet:setopts(Socket, [{active, once}]),
			receive_reply(),
            quiet_loop(ListenSocket);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

%%------------------------------------------------------------------------------
%% Opens tcp socket and calls receive_reply()
%%------------------------------------------------------------------------------
connection_request(ServerHost, Port) ->
    case gen_tcp:connect(ServerHost, Port,
                                 [binary, {packet, 2}]) of
        {ok, Sock} ->
            ok = gen_tcp:send(Sock, term_to_binary([{releases, release_handler:which_releases()}])),
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
            case binary_to_term(Binary) of
                {Request, Body} ->
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
handle(get_data, Body) ->
    io:format("get data ~p ~n", [Body]);
handle(download_tar, Name) ->
    sup_beagle_download:download_tar(Name);
handle(_AnythingElse, _Body) ->
    io:format("what a terrible failure").


