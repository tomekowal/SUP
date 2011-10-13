-module(sup_beagle_management).
-include("sup_beagle.hrl").
-export([start_link/0, loop/0, trigger_session/1]).

%%------------------------------------------------------------------------------
%% @doc Starts management client process.
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    Pid = spawn_link(?MODULE, loop, []),
    register(management_client, Pid),
    trigger_session(startup),
    {ok, NotifyInterval} = sup_beagle_config:get(periodic_notify_interval),
    {ok, _TRef} = timer:apply_interval(NotifyInterval*1000, ?MODULE,
                                      trigger_session, [periodic_notify]),
    {ok, Pid}.

%%------------------------------------------------------------------------------
%% @doc Triggers a session with management server for a given reason.
%% @end
%%------------------------------------------------------------------------------
trigger_session(Reason) ->
    management_client ! {perform_session, Reason}.

%%------------------------------------------------------------------------------
%% @doc Management client main loop. Message {perform_session, Reason}
%% makes the management client process initiate the session. Example reasons
%% include connection request, periodic notify, scheduled notify etc. These
%% are triggered externally.
%% @end
%%------------------------------------------------------------------------------
loop() ->
    receive
        {perform_session, Reason} ->
            case (catch session(Reason)) of
                ok ->
                    ok;
                Exception ->
                    io:format("Session failed: ~p~n", [Exception])
            end,
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
session(Reason) ->
    {ok, Host} = sup_beagle_config:get(management_host),
    {ok, Port} = sup_beagle_config:get(management_port),
    Options = [binary, {packet, 4}, {active, false}],
    {ok, Socket} = gen_tcp:connect(Host, Port, Options),
    Message = init_session_message(Reason),
    gen_tcp:send(Socket, term_to_binary(Message)),
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
%% Creates initial session message sent to management server based on reason.
%%------------------------------------------------------------------------------
init_session_message(Reason) ->
    {ok, Identity} = sup_beagle_config:get(identity),
    Releases = release_handler:which_releases(),
    #inform{identity = Identity, reason = Reason, releases = Releases}.

%%------------------------------------------------------------------------------
%% Job handlers.
%%
%% Argument is the job sent by the server. Return value (or caught
%% exception) will be sent to the server as result.
%% -----------------------------------------------------------------------------
handle_job({get_release, Name}) ->
    ok = sup_beagle_download:download_tar(Name),
    {ok, Vsn} = release_handler:unpack_release(Name),
    {ok, Vsn};
handle_job({update_to_release, Vsn}) ->
    {ok, _OtherVsn, _Descr} = release_handler:install_release(Vsn),
    ok = release_handler:make_permanent(Vsn),
    {ok, release_handler:which_releases()};
handle_job(_Job) ->
    ok.
