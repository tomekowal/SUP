-module(sup_beagle_management).
-export([start_link/0, loop/0, connection_request/0]).

%%------------------------------------------------------------------------------
%% @doc Starts management client process.
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    Pid = spawn_link(?MODULE, loop, []),
    register(management_client, Pid),
    {ok, Pid}.

%%------------------------------------------------------------------------------
%% @doc Triggers a session with management server due to a connection request.
%% @end
%%------------------------------------------------------------------------------
connection_request() ->
    management_client ! {perform_session, connection_request}.

%%------------------------------------------------------------------------------
%% @doc Management client main loop. Message {perform_session, Reason}
%% makes the management client process initiate the session. Example reasons
%% include connection request, periodic notify, scheduled notify etc. These
%% are triggered externally.
%% @end
%%------------------------------------------------------------------------------
loop() ->
    receive
        {perform_session, _Reason} ->
            session(),
            ?MODULE:loop();
        stop ->
            ok
    end.

%%------------------------------------------------------------------------------
%% Initiates a session with management server.
%%
%% A session is a conversation with management server spanning one TCP
%% connection. Client initiates the session by sending an initial message to the
%% server. This message should contain general state of the client
%% (e.g. software versions) and some information about why the session was
%% initiated.
%%
%% The server will then start sending jobs to the client. Client handles each
%% job and sends the server its result (exceptions from job handlers are caught
%% and sent) and the waits for more jobs. After the server sends 'finished'
%% message, session is finished and connection is closed.
%% -----------------------------------------------------------------------------
session() ->
    {ok, Host} = application:get_env(sup_beagle, management_host),
    {ok, Port} = application:get_env(sup_beagle, management_port),
    Options = [binary, {packet, 4}, {active, false}],
    {ok, Socket} = gen_tcp:connect(Host, Port, Options),
    Releases = release_handler:which_releases(),
    gen_tcp:send(Socket, term_to_binary(Releases)),
    session_loop(Socket).

%%------------------------------------------------------------------------------
%% Session main loop.
%%------------------------------------------------------------------------------
session_loop(Socket) ->
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    case binary_to_term(Packet) of
        finished ->
            gen_tcp:close(Socket),
            ok;
        Job ->
            Response = (catch handle_job(Job)),
            gen_tcp:send(Socket, term_to_binary(Response)),
            session_loop(Socket)
    end.

%%------------------------------------------------------------------------------
%% Job handlers.
%%
%% Argument is the job sent by the server. Return value (or caught
%% exception) will be sent to the server as result.
%% -----------------------------------------------------------------------------
handle_job({get_release, Name}) ->
    ok = sup_beagle_download:download_tar(Name),
    {ok, _Vsn} = release_handler:unpack_release(Name),
    ok;
handle_job({update_to_release, Vsn}) ->
    {ok, OtherVsn, Descr} = release_handler:install_release(Vsn),
    ok = release_handler:make_permanent(Vsn),
    {ok, OtherVsn, Descr};
handle_job(Job) ->
    io:format("Unknown job: ~p~n", [Job]),
    ok.
