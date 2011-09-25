-module(sup_server_management).
-export([start_link/0, loop/1, begin_session/1]).
-export([chain_handler/4, print_result_handler/4]).

start_link() ->
    {ok, ServerSocket} = gen_tcp:listen(5678, [binary, {packet, 4}, {active, false}]),
    {ok, spawn_link(?MODULE, loop, [ServerSocket])}.

loop(ServerSocket) ->
    {ok, Socket} = gen_tcp:accept(ServerSocket),
    spawn(?MODULE, begin_session, [Socket]),
    ?MODULE:loop(ServerSocket).

begin_session(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Message} ->
            {SessionData, Handlers} = init_session(Message),
            session_loop(Socket, SessionData, Handlers);
        {error, _Reason} ->
            broken
    end.

session_loop(Socket, _SessionData, []) ->
    gen_tcp:send(Socket, term_to_binary(finished)),
    gen_tcp:close(Socket),
    finished;
session_loop(Socket, SessionData, [Handler | PendingHandlers]) ->
    {Job, Module, Function, Extra} = Handler,
    gen_tcp:send(Socket, term_to_binary(Job)),
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            Message = binary_to_term(Packet),
            MoreHandlers = apply(Module, Function, [Job, Message, SessionData, Extra]),
            session_loop(Socket, SessionData, MoreHandlers++PendingHandlers);
        {error, _Reason} ->
            gen_tcp:close(Socket),
            broken
    end.

init_session(_Message) ->
    {none,
     [
      {job1, ?MODULE, print_result_handler, none},
      {job2, ?MODULE, print_result_handler, none},
      {job3, ?MODULE, print_result_handler, none},
      {{download_tar, "beagle_3"}, ?MODULE, print_result_handler, none}
     ]
    }.

chain_handler(_Job, _Message, _SessionData, 0) ->
    [];
chain_handler(Job, Message, _SessionData, Num) ->
    io:format("Handling ~p ~p ~p~n", [Job, Message, Num]),
    [{Job, ?MODULE, chain_handler, Num-1}].

print_result_handler(Job, Message, SessionData, Extra) ->
    io:format("Job: ~p~n", [Job]),
    io:format("Result: ~p~n", [Message]),
    io:format("Session data: ~p~n", [SessionData]),
    io:format("Extra data: ~p~n", [Extra]),
    [].
