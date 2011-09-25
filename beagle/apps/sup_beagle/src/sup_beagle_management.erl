-module(sup_beagle_management).
-export([start_link/0, loop/0]).

start_link() ->
    Pid = spawn_link(?MODULE, loop, []),
    register(management_client, Pid),
    {ok, Pid}.

loop() ->
    receive
        {perform_session, _Reason} ->
            session(),
            ?MODULE:loop();
        stop ->
            ok
    end.

session() ->
    {ok, Host} = application:get_env(sup_beagle, management_host),
    {ok, Port} = application:get_env(sup_beagle, management_port),
    Options = [binary, {packet, 4}, {active, false}],
    {ok, Socket} = gen_tcp:connect(Host, Port, Options),
    Releases = release_handler:which_releases(),
    gen_tcp:send(Socket, term_to_binary(Releases)),
    session_loop(Socket).

session_loop(Socket) ->
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    case binary_to_term(Packet) of
        finished ->
            gen_tcp:close(Socket),
            ok;
        Job ->
            Response = handle_job(Job),
            gen_tcp:send(Socket, term_to_binary(Response)),
            session_loop(Socket)
    end.

handle_job({download_tar, Name}) ->
    io:format("Downloading release ~p~n", [Name]),
    sup_beagle_download:download_tar(Name);
handle_job(Job) ->
    io:format("Unknown job: ~p~n", [Job]),
    ok.
